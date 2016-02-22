package dictractor.extractors.proposed

import java.io.File

import dict.{Alias, GenderedAlias, Character}
import dictractor.coref.{AliasCorefMention, NormalizedCorefResoluter}
import name._
import story.{StoryOverseer, StoryId}
import story.StorySet._
import text.{TOK, TextManager}

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge


/**
 * Transforms a character graph.
 */
trait CharacterGraphTransform {

  /** Transform id. */
  val id: String

  /**
   * Transforms the given character graph.
   * @param g Character graph.
   * @param storyId Option of a story id (if needed), defaults to None.
   * @param nodeWeightAssigner Option of a node weight assigner (if needed for weighting newly added nodes), defaults to
   *                           None.
   * @return Transformed character graph.
   */
  def transform(g: CharacterGraph,
                storyId: Option[StoryId] = None,
                nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph

}

object CharacterGraphTransform {

  /**
   * Applies a sequence of character graph transforms to the given character graph.
   * @param g Initial character graph.
   * @param transforms Sequence of character graph transforms to apply.
   * @param storyId Option of a story id (if needed).
   * @param nodeWeightAssigner Option of a node weight assigner (if needed by a transform).
   * @return Transformed character graph.
   */
  def apply(g: CharacterGraph,
            transforms: Seq[CharacterGraphTransform],
            storyId: Option[StoryId],
            nodeWeightAssigner: Option[NodeWeightAssigner]): CharacterGraph =
    transforms.foldLeft(g)({ case (h, t) => t.transform(h, storyId, nodeWeightAssigner)})

}

/**
 * Resolves the given anti-edges.
 */
object ResolveAntiEdges extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "rae"

  /**
   * Returns the resolved character graph resulting from the resolution of its anti-edges.
   * @param g Character graph.
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Resolved character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId] = None,
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    // Converts a character graph edge to a scalax UnDiEdge.
    def toUniDiEdge(e: CharacterGraphEdge): UnDiEdge[AliasNode] = UnDiEdge(e.node1, e.node2)

    // Returns the list of nodes on the shortest path between the given alias nodes. If no such path exists, then None is
    // returned.
    def shortestPath(g: Graph[AliasNode, UnDiEdge], a1: AliasNode, a2: AliasNode): Option[List[AliasNode]] = {
      val a1Node = g.get(a1)
      val a2Node = g.get(a2)
      val shortestPath = a1Node.shortestPathTo(a2Node)
      shortestPath match {
        case Some(p) => Some(p.nodes.map(s => new AliasNode(s.alias)).toList)
        case None => None
      }
    }

    // Strip the end nodes for the given path.
    def stripEnds(path: List[AliasNode]): List[AliasNode] = path.drop(1).dropRight(1)

    // Find the conflict node on the given conflict path, i.e. the node with shortest alias span.
    def findConflictNode(conflictPath: List[AliasNode]): AliasNode =
      conflictPath.minBy(_.getSpan.length)

    // Disconnect the given alias node from the given character graph, i.e. remove all edges incident on the node.
    def disconnectNode(g: Graph[AliasNode, UnDiEdge], a: AliasNode): Graph[AliasNode, UnDiEdge] =
      g.get(a).neighbors.map(s => new AliasNode(s.alias)).foldLeft(g)({ case (h, n) => h - UnDiEdge(a, n) })

    // Resolves the given anti-edge recursively.
    def resolveAntiEdge(h: Graph[AliasNode, UnDiEdge], ae: CharacterAntiEdge): Graph[AliasNode, UnDiEdge] =
      shortestPath(h, ae.node1, ae.node2) match {
        case Some(p) =>
          resolveAntiEdge(disconnectNode(h, findConflictNode(stripEnds(p))), ae)
        case None => h
      }

    val resolvedG: Graph[AliasNode, UnDiEdge] =
      g.antiEdges.foldLeft (Graph.from(g.nodes, g.edges.map(toUniDiEdge))) (resolveAntiEdge)

    val resolvedNodes: Set[AliasNode] = resolvedG.nodes.map(n => AliasNode(n.alias)).toSet

    new CharacterGraph(
      g.nodes.filter(resolvedNodes.contains),
      g.edges.filter(e => resolvedG.find(toUniDiEdge(e)).nonEmpty),
      // Filter those anti-edges with ends in the resulting resolved character graph.
      g.antiEdges.filter(ae => resolvedNodes.contains(ae.node1) && resolvedNodes.contains(ae.node2))
    )
  }

}

/**
 * Prunes singleton alias nodes with weight less than the specified threshold.
 * @param t Node weight threshold.
 */
class PruneSingletonNodes(t: Int) extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "ps" + t

  /**
   * Transforms the given character graph by pruning singleton nodes with weight less than a threshold.
   * @param g Character graph (Nodes must be weighted).
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId] = None,
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val prunedNodes: Set[AliasNode] = g.nodes.filterNot(n => {
      val nodeWeight = n.weight match {
        case Some(w) => w
        case None => throw new RuntimeException("Alias node " + n + " must be assigned a weight.")
      }

      g.degree(n) == 0 && nodeWeight < this.t
    }).toSet

    new CharacterGraph(prunedNodes,
      // Filter edges that have end nodes in the pruned set of nodes.
      g.edges.filter(e => prunedNodes.contains(e.node1) && prunedNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the pruned set of nodes.
      g.antiEdges.filter(ae => prunedNodes.contains(ae.node1) && prunedNodes.contains(ae.node2)))
  }

}

/**
 * Removes singleton alias nodes that have a name that's a variant of at least one other named alias node in the graph,
 * while respecting gender.
 */
object RemoveAmbiguousNameNodes extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "rmambig"

  /**
   * Transforms the given character graph by removing singleton alias nodes that have a name that's a variant of at
   * least one other named alias node in the graph (while respecting gender).
   * @param g Character graph (Alias nodes must have gender).
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId] = None,
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val characters: Set[Character] = g.toCharacters
    // Aliases belonging to singleton characters.
    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases)

    // Checks whether the given alias can be merged with the given character based on naming rules and gender.
    def canBeMerged(a: Alias, c: Character): Boolean = {
      val span: String = a.span
      val gender: Gender = a match {
        case a: GenderedAlias => a.gender
        case b => throw new RuntimeException("Alias " + b + " must be gendered.")
      }

      val hasFemaleTitle = NameTitles.hasFemaleTitle(span)
      val hasSpousalFemaleTitle = NameTitles.hasTitle(span, "Mrs.")
      val hasNonSpousalFemaleTitle = NameTitles.hasTitle(span, "Ms.") || NameTitles.hasTitle(span, "Miss")
      val hasMaleTitle = NameTitles.hasMaleTitle(span)
      val hasStrictFemaleName = NameGazetteer.hasStrictFemaleName(span)
      val hasStrictMaleName = NameGazetteer.hasStrictMaleName(span)

      // If the given alias has a female title but the character has an alias with a male title, then return false.
      if (hasFemaleTitle &&
        !hasMaleTitle &&
        c.aliases.exists(a => NameTitles.hasMaleTitle(a.span) && !NameTitles.hasFemaleTitle(a.span)))
        false
      else if (hasSpousalFemaleTitle &&
        c.aliases.exists(a => NameTitles.hasTitle(a.span, "Ms.") || NameTitles.hasTitle(a.span, "Miss")))
        false
      else if (hasNonSpousalFemaleTitle && c.aliases.exists(a => NameTitles.hasTitle(a.span, "Mrs.")))
        false
      // If the given alias has a male title but the character has an alias with a female title, then return false.
      else if (hasMaleTitle &&
        !hasFemaleTitle &&
        c.aliases.exists(a => NameTitles.hasFemaleTitle(a.span) && !NameTitles.hasMaleTitle(a.span)))
        false
      // If the given alias has a strictly female name but the character has an alias with a strictly male name,
      // then return false.
      else if (hasStrictFemaleName &&
        !hasStrictMaleName &&
        c.aliases.exists(a => NameGazetteer.hasStrictMaleName(a.span) &&
          !NameGazetteer.hasStrictFemaleName(a.span)))
        false
      // If the given alias has a strictly male name but the character has an alias with a strictly female name,
      // then return false.
      else if (hasStrictMaleName &&
        !hasStrictFemaleName &&
        c.aliases.exists(a => NameGazetteer.hasStrictFemaleName(a.span) &&
          !NameGazetteer.hasStrictMaleName(a.span)))
        false

      val isNameVariant = c.aliases.exists(b => {
        a.span != b.span && Name.areVariants(a.span)(b.span)
      })

      if (isNameVariant) {
        if (gender == FEMALE)
          c.aliases.forall(a => {
            val g = a match {
              case b: GenderedAlias => b.gender
              case b => throw new RuntimeException("Alias " + b + " must be gendered.")
            }

            g != MALE
          })
        else if (gender == MALE)
          c.aliases.forall(a => {
            val g = a match {
              case b: GenderedAlias => b.gender
              case b => throw new RuntimeException("Alias " + b + " must be gendered.")
            }

            g != FEMALE
          })
        else
          true
      } else
        false
    }

    val unambiguousNodes: Set[AliasNode] = g.nodes
      // Only singleton character aliases can be ambiguous.
      .filterNot(n => singletons.contains(n.alias) &&
        // Filter out those that can be merged with a character with more than one alias
        (characters.filter(_.size > 1).exists(c => canBeMerged(n.alias, c)) ||
          // Filter out those the can be merged with more than one singleton.
          characters.filter(_.size == 1).count(c => c.aliases.head.span != n.getSpan && canBeMerged(n.alias, c)) > 1))
      .toSet

    new CharacterGraph(
      g.nodes.filter(unambiguousNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => unambiguousNodes.contains(e.node1) && unambiguousNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => unambiguousNodes.contains(ae.node1) && unambiguousNodes.contains(ae.node2))
    )
  }

}

/**
 * Removes singleton alias nodes that are plural names of at least one other named alias node in the graph.
 */
object RemovePluralNameNodes extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "plur"

  /**
   * Transforms the given character graph by removing singleton alias nodes that are plural names of at least one other
   * named alias node in the graph.
   * @param g Character graph.
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId] = None,
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val characters: Set[Character] = g.toCharacters
    // Aliases belonging to singleton characters after stripping name titles.
//    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases).map(a =>
//      new Alias(NameTitles.stripTitles(a.span))).filterNot(_.span.isEmpty)

    val isPlural = (n: AliasNode, nodes: Iterable[AliasNode]) => {
      // Perform naive check if the given name is in plural form (will allow non-plural names ending in 's' or 'es').
      if (n.getSpan.endsWith("es")) {
        // Simply drop the ending 'es'.
        val nonPluralForm: String = NameTitles.stripTitles(n.getSpan.substring(0, n.getSpan.length - 2))

        nodes.exists(m => n.getSpan != m.getSpan && m.getSpan.matches("(^|.*\\W)" + nonPluralForm + "$"))
      } else if (n.getSpan.endsWith("s")) {
        // Simply drop the ending 's'.
        val nonPluralForm: String = NameTitles.stripTitles(n.getSpan.substring(0, n.getSpan.length - 1))

        nodes.exists(m => n.getSpan != m.getSpan && m.getSpan.matches("(^|.*\\W)" + nonPluralForm + "$"))
      } else
        false
    }

    val filteredNodes: Set[AliasNode] = g.nodes.filterNot(n => isPlural(n, g.nodes)).toSet

    new CharacterGraph(
      g.nodes.filter(filteredNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => filteredNodes.contains(e.node1) && filteredNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => filteredNodes.contains(ae.node1) && filteredNodes.contains(ae.node2))
    )
  }
}

/**
 * Removes singleton alias nodes that are name titles for alias nodes in the graph.
 */
object RemoveDanglingNameTitles extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "rmnt"

  /**
   * Transforms the given character graph by removing singleton alias nodes that are name titles for alias nodes.
   * @param g Character graph.
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId],
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val characters: Set[Character] = g.toCharacters
    val aliases: Set[Alias] = characters.flatMap(_.aliases)
    // Aliases belonging to singleton characters.
    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases)

    val isDanglingNameTitle = (n: AliasNode) =>
      singletons.contains(n.alias) && NameTitles.isTitle(n.getSpan) &&
        aliases.exists(a => a.span != n.getSpan && NameTitles.hasTitle(a.span, n.getSpan))

    val filteredNodes: Set[AliasNode] = g.nodes.filterNot(isDanglingNameTitle).toSet

    new CharacterGraph(
      g.nodes.filter(filteredNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => filteredNodes.contains(e.node1) && filteredNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => filteredNodes.contains(ae.node1) && filteredNodes.contains(ae.node2))
    )
  }

}

/**
 * Removes singleton alias nodes that have spans only used possessively in the provided story text.
 */
object RemovePossessiveNames extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "rmposs"

  /**
   * Transforms the given character graph by removing singleton alias nodes that have spans only used possessively in
   * the provided story text.
   * @param g Character graph.
   * @param storyId Option of a story id (Required).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId],
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val text: String = storyId match {
      case Some(sid) => TextManager.getText(sid, TOK)
      case None => throw new RuntimeException("Story id required.")
    }

    val characters: Set[Character] = g.toCharacters
    // Aliases belonging to singleton characters.
    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases)

    def isOnlyPossessive(a: Alias): Boolean = ("(^|\\W)" + a.span + "\\s's").r.findFirstIn(text).isDefined &&
      ("(^|\\W)(" + a.span + "\\W[^('s)]|" + a.span + "$)").r.findFirstIn(text).isEmpty

    val nonPossessiveNodes: Set[AliasNode] =
      // Make sure that the "only-possessive" is not a person by verifying against the name titles list.
      g.nodes.filterNot(n => singletons.contains(n.alias) && !NameTitles.hasTitle(n.getSpan) &&
        isOnlyPossessive(n.alias)).toSet

    new CharacterGraph(
      g.nodes.filter(nonPossessiveNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => nonPossessiveNodes.contains(e.node1) && nonPossessiveNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => nonPossessiveNodes.contains(ae.node1) && nonPossessiveNodes.contains(ae.node2))
    )
  }

}

// TODO
object HandleLowerCaseNameTitles extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "lownt"

  /**
   * Transforms the given character graph.
   * @param g Character graph.
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId],
                         nodeWeightAssigner: Option[NodeWeightAssigner]): CharacterGraph = {
    val characters: Set[Character] = g.toCharacters

    def capitalize(s: String): String =
      s.split("\\s+").map(t => t(0).toUpper + (if (t.length > 1) t.substring(1, t.length) else "")).mkString(" ")

    def countMatchingCharacters(a: Alias): Int = {
      val capitalizedSpan = capitalize(a.span)
      if (NameTitles.isTitle(capitalizedSpan)) {
        characters.count(_.aliases.exists(a => NameTitles.hasTitle(a.span, capitalizedSpan)))
      } else
        0
    }

    def matchesSingleCharacters(a: Alias): Boolean = countMatchingCharacters(a) == 1
    def matchesMultipleCharacters(a: Alias): Boolean = countMatchingCharacters(a) > 1

    def getMatchingCharacterAliases(a: Alias): Set[Alias] = {
      val capitalizedSpan = capitalize(a.span)
      characters.find(_.aliases.exists(a => NameTitles.hasTitle(a.span, capitalizedSpan))).get.aliases
    }

    val filteredNodes: Set[AliasNode] = g.nodes.filterNot(n => matchesMultipleCharacters(n.alias)).toSet
    val newEdges: Set[CharacterEdge] = g.nodes
      .filter(n => matchesSingleCharacters(n.alias))
      .flatMap(n => getMatchingCharacterAliases(n.alias).map(a => CharacterEdge(n, AliasNode(a)))).toSet

    new CharacterGraph(
      g.nodes.filter(filteredNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => filteredNodes.contains(e.node1) && filteredNodes.contains(e.node2)).toSet | newEdges,
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => filteredNodes.contains(ae.node1) && filteredNodes.contains(ae.node2))
    )
  }
}

/**
 * Incorporates new singleton alias nodes based on verb-extracted aliases.
 */
object IncorporateVerbExtractedAliases extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "verb"

  private val NAME = "verb-extracted-aliases"

  // Loads the verb-extracted aliases as set from the .tsv file located by the given filepath.
  private def loadVerbExtractedAliases(filepath: String) =
    io.Source.fromFile(new File(filepath))
      .getLines()
      // File should be a .tsv with the names in the first column.
      .map(_.split("\t")(0))
      // Filter out 'NAR's, 'NARRATOR's, and 'Gutenberg'.
      .filterNot(s => s.contains("NAR") | s.toLowerCase.contains("gutenberg"))
      // Map to Alias.
      .map(new Alias(_))
      .toSet

  // TODO
  def areStrongNameVariants(a1: Alias, a2: Alias): Boolean = {
    val span1: String = a1.span
    val gender1: Gender = a1 match {
      case a: GenderedAlias => a.gender
      case b => throw new RuntimeException("Alias " + b + " must be gendered.")
    }

    val span2: String = a2.span
    val gender2: Gender = a2 match {
      case a: GenderedAlias => a.gender
      case b => throw new RuntimeException("Alias " + b + " must be gendered.")
    }

    val hasFemaleTitle1 = NameTitles.hasFemaleTitle(span1)
    val hasSpousalFemaleTitle1 = NameTitles.hasTitle(span1, "Mrs.")
    val hasNonSpousalFemaleTitle1 = NameTitles.hasTitle(span1, "Ms.") || NameTitles.hasTitle(span1, "Miss")
    val hasMaleTitle1 = NameTitles.hasMaleTitle(span1)
    val hasStrictFemaleName1 = NameGazetteer.hasStrictFemaleName(span1)
    val hasStrictMaleName1 = NameGazetteer.hasStrictMaleName(span1)

    val hasFemaleTitle2 = NameTitles.hasFemaleTitle(span2)
    val hasSpousalFemaleTitle2 = NameTitles.hasTitle(span2, "Mrs.")
    val hasNonSpousalFemaleTitle2 = NameTitles.hasTitle(span2, "Ms.") || NameTitles.hasTitle(span2, "Miss")
    val hasMaleTitle2 = NameTitles.hasMaleTitle(span2)
    val hasStrictFemaleName2 = NameGazetteer.hasStrictFemaleName(span2)
    val hasStrictMaleName2 = NameGazetteer.hasStrictMaleName(span2)

    if (hasFemaleTitle1 && !hasMaleTitle1 && hasMaleTitle2 && !hasFemaleTitle2)
      false
    else if (hasSpousalFemaleTitle1 && hasNonSpousalFemaleTitle2)
      false
    else if (hasNonSpousalFemaleTitle1 && hasSpousalFemaleTitle2)
      false
    else if (hasMaleTitle1 && !hasFemaleTitle1 && hasFemaleTitle2 && !hasMaleTitle2)
      false
    else if (hasStrictFemaleName1 && !hasStrictMaleName1 && hasStrictMaleName2 && !hasStrictFemaleName2)
      false
    else if (hasStrictMaleName1 && !hasStrictFemaleName1 && hasStrictFemaleName2 && !hasStrictMaleName2)
      false

    if (Name.areVariants(span1)(span2)) {
      if (gender1 == FEMALE)
          gender2 != MALE
      else if (gender1 == MALE)
          gender2 != FEMALE
      else
        true
    } else
      false
  }

  /**
   * Transforms the given character graph.
   * @param g Character graph.
   * @param storyId Option of a story id (Required).
   * @param nodeWeightAssigner Option of a node weight assigner (Required).
   * @return Transformed character graph.
   */
  // TODO
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId],
                         nodeWeightAssigner: Option[NodeWeightAssigner]): CharacterGraph = {
    val filepath: String = storyId match {
      case Some(sid) => sid match {
        case StoryId(MOONSTONE, None) =>
          StoryOverseer.getPath(MOONSTONE) + File.separator + this.NAME + ".tsv"
        case StoryId(PRIDE_AND_PREJUDICE, None) =>
          StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + this.NAME + ".tsv"
        case StoryId(SHERLOCK_HOLMES, Some(n)) =>
          StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + this.NAME + File.separator + n + ".tsv"
        case StoryId(SILVER_STANDARD, Some(n)) =>
          StoryOverseer.getPath(SILVER_STANDARD) + File.separator + this.NAME + File.separator + n + ".tsv"
        case StoryId(ELSON, Some(n)) =>
          StoryOverseer.getPath(ELSON) + File.separator + this.NAME + File.separator + n + ".tsv"
        case StoryId(PROJECT_GUTENBERG, Some(n)) =>
          StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + this.NAME + File.separator + n + ".tsv"
      }
      case None => throw new RuntimeException("Story id required.")
    }

    val characters: Set[Character] = g.toCharacters
    val aliases: Set[Alias] = characters.flatMap(_.aliases)

    // Aliases belonging to singleton characters.
    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases)
    // Titled or named singleton aliases.
    val titledOrNamedSingletons: Set[Alias] =
      singletons.filter(a => NameTitles.hasTitle(a.span) || NameGazetteer.hasName(a.span))
//    val remainingSingletons = singletons &~ titledOrNamedSingletons

//    val remainingAliases = aliases &~ (singletons &~ titledOrNamedSingletons)

    val normCoref: List[List[AliasCorefMention]] = NormalizedCorefResoluter.resolute(storyId.get)

    val finalAliases = aliases &~ (singletons &~ titledOrNamedSingletons)
    val finalNodes = finalAliases.map(AliasNode)

    val verbExtractedAliases: Set[GenderedAlias] = this.loadVerbExtractedAliases(filepath)
      .diff(finalAliases)
      .filterNot(a => normCoref.exists(c => c.exists(_.span == a.span) &&
      c.exists(m => aliases.exists(m.span == _.span))))
      .map(a => new GenderedAlias(a.span, Gender.assign(a.span)))

//    val matchingNameVariantSingletons =
//      remainingSingletons.filter(s => verbExtractedAliases.exists(v => areStrongNameVariants(s, v) &&
//        remainingAliases.forall(r => !areStrongNameVariants(v, r))))

    val verbExtractedNodes: Set[AliasNode] = verbExtractedAliases.map(new AliasNode(_))

    val assigner: NodeWeightAssigner = nodeWeightAssigner match {
      case Some(a) => a
      case _ => throw new RuntimeException("Node weight assigner required.")
    }
    assigner.assign(storyId.get, verbExtractedNodes)

    new CharacterGraph(
      g.nodes.filter(finalNodes.contains).toSet | verbExtractedNodes,
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => finalNodes.contains(e.node1) && finalNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => finalNodes.contains(ae.node1) && finalNodes.contains(ae.node2))
    )

  }

}

/**
 * Filters out singleton characters belonging to the stop list.
 */
object FilterNotStopListCharacters extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "stop"

  /** Set of stop character aliases. */
  val stopCharacterAliases: Set[String] =
    Set("Aye", "God", "chap", "child", "companion", "fellow", "friend", "lad", "visitor", "youth")

  /**
   * Transforms the given character graph by filtering out singleton character aliases belong to the stop set.
   * @param g Character graph.
   * @param storyId Option of a story id (Not needed).
   * @param nodeWeightAssigner Option of a node weight assigner (Not needed).
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId] = None,
                         nodeWeightAssigner: Option[NodeWeightAssigner] = None): CharacterGraph = {
    val characters: Set[Character] = g.toCharacters
    // Aliases belonging to singleton characters.
    val singletons: Set[Alias] = characters.filter(_.size == 1).flatMap(_.aliases)

    def isStop(a: Alias): Boolean = stopCharacterAliases.contains(a.span)

    val nonStopNodes: Set[AliasNode] = g.nodes.filterNot(n => singletons.contains(n.alias) && isStop(n.alias)).toSet

    new CharacterGraph(
      g.nodes.filter(nonStopNodes.contains),
      // Filter edges that have end nodes in the filtered set of nodes.
      g.edges.filter(e => nonStopNodes.contains(e.node1) && nonStopNodes.contains(e.node2)),
      // Filter anti-edges that have end nodes in the filtered set of nodes.
      g.antiEdges.filter(ae => nonStopNodes.contains(ae.node1) && nonStopNodes.contains(ae.node2))
    )
  }
}

// TODO
object ExtractConjunctionDialogueAliases extends CharacterGraphTransform {

  /** Transform id. */
  override val id: String = "cdals"

  /**
   * Transforms the given character graph.
   * @param g Character graph.
   * @param storyId Option of a story id (if needed), defaults to None.
   * @param nodeWeightAssigner Option of a node weight assigner (if needed for weighting newly added nodes), defaults to
   *                           None.
   * @return Transformed character graph.
   */
  override def transform(g: CharacterGraph,
                         storyId: Option[StoryId],
                         nodeWeightAssigner: Option[NodeWeightAssigner]): CharacterGraph = storyId match {
    case Some(sid) =>
      def isCapitalized(s: String): Boolean = !s.isEmpty && s(0).isUpper

      val antiEdges: Set[CharacterAntiEdge] = ConjunctionDialogueAntiEdgeLoader.get(sid)

      val nodes = g.nodes.toSet
      val aliasSpans: Set[String] = nodes.map(_.alias.span)

      val extractedAliases: Set[GenderedAlias] = antiEdges
        .map(ae => (ae.node1.getSpan, ae.node2.getSpan))
        .filter(p => (aliasSpans.contains(p._1) && isCapitalized(p._2)) ||
          (aliasSpans.contains(p._2) && isCapitalized(p._1)))
        .flatMap(p => List(p._1, p._2))
        .diff(aliasSpans)
        .map(s => new GenderedAlias(s, Gender.assign(s)))

      new CharacterGraph(
        g.nodes.toSet | extractedAliases.map(AliasNode(_)),
        g.edges,
        // Filter anti-edges that have end nodes in the filtered set of nodes.
        g.antiEdges.toSet | antiEdges.filter(ae => nodes.contains(ae.node1) && nodes.contains(ae.node2))
      )
    case None => throw new RuntimeException("Story id required.")
  }

}

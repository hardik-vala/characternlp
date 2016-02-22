package dictractor.extractors.proposed

import dict.GenderedAlias
import lang.{Pronouns, Determiners}
import name._
import story.StoryId
import utils.Utils


/**
 * Rule for a character graph edge.
 */
trait CharacterGraphEdgeRule {

  /** Rule id. */
  val id: String

  /**
   * Applies the rule to the given character edge, return a yes/no according to whether the rule is satisfied.
   * @param edge Character edge.
   * @param storyId Option of story Id (if needed), defaulting to None.
   * @return True if the rule is satisfied, false otherwise.
   */
  def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean

}

/** Positive rule for character edges, so application of the rule returns true if the given edge should be included
  * in the character graph. */
trait CharacterEdgeRule extends CharacterGraphEdgeRule
/** Negative rule for character edges, so application of the rule returns true if the given edge should be considered an
  * anti-edge in the character graph. */
trait CharacterAntiEdgeRule extends CharacterGraphEdgeRule

/**
 * Rule for thresholding on edge weight.
 * @param t Threshold.
 */
class EdgeWeightRule(t: Int) extends CharacterEdgeRule {

  // Threshold must be non-zero.
  require(t >= 0)

  /** Rule id. */
  override val id: String = "edge-" + t

  /**
   * Returns true if the weight of the edge is at least the threshold, and false otherwise.
   * @param edge Character edge.
   * @param storyId Option of story Id (Not needed).
   * @return True if the weight of the edge is at least the threshold, and false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean = edge.weight match {
    case Some(w) => w >= this.t
    case _ => throw new IllegalStateException("Edge has no weight.")
  }

}

/**
 * Rule for testing whether the aliases of the nodes are name variants of each other.
 */
object NameVariantRule extends CharacterEdgeRule {

  /** Rule id. */
  override val id: String = "name"

  /**
   * Returns true if the aliases of the incident edge nodes are name variants of each other, and false otherwise.
   * @param edge Character edge.
   * @param storyId Option of story Id (Not needed).
   * @return True if the aliases of the incident edge nodes are name variants of each other, and false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean =
    Name.areVariants (edge.node1.getSpan) (edge.node2.getSpan)

}

/**
 * Rule for checking whether the aliases of the nodes are hypocorisms of each other.
 */
object HypocorismRule extends CharacterEdgeRule {

  /** Rule id. */
  override val id: String = "hypo"

  /**
   * Returns true if the aliases of the incident edge nodes are hypocorisms of each other, and false otherwise.
   * @param edge Character edge.
   * @param storyId Option of story Id (Not needed).
   * @return True if the aliases of the incident edge nodes are hypocorisms of each other, and false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean =
    Hypocorisms.areHypocorisms (edge.node1.getSpan) (edge.node2.getSpan)
}

/**
 * Anti-rule for testing whether the aliases of incident nodes have the same last name but differing first names.
 */
object FamilyNameAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "fn"

  /**
   * Checks whether the alias spans of the incident nodes have the same last name but differing first names.
   * @param edge Character edge.
   * @param storyId Option of story Id (Not needed).
   * @return True if the alias spans have the same last name but differing first names, false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean = {
    val span1: String = edge.node1.getSpan
    val span2: String = edge.node2.getSpan

    // If any of the spans starts with a determiner or pronoun, automatically return false.
    if (Determiners.startsWithDeterminer(span1) ||
        Determiners.startsWithDeterminer(span2) ||
        Pronouns.startsWithPronoun(span1) ||
        Pronouns.startsWithPronoun(span2))
      false
    else {
      // Strip the titles from the spans and extract the name portions by filtering out any lowercase tokens.
      val nameTokens1 = NameTitles.stripTitles(span1).split("\\s+").filterNot(Utils.isLowerCase)
      val nameTokens2 = NameTitles.stripTitles(span2).split("\\s+").filterNot(Utils.isLowerCase)

      // There must be at least two name tokens (e.g. first and last names), otherwise immediately return false.
      if (nameTokens1.length > 1 && nameTokens2.length > 1) {
        // Last names are the last name token.
        val lastName1 = nameTokens1(nameTokens1.length - 1)
        val lastName2 = nameTokens2(nameTokens2.length - 1)

        // Names without the last name.
        val restName1 = nameTokens1.slice(0, nameTokens1.length - 1).mkString(" ")
        val restName2 = nameTokens2.slice(0, nameTokens2.length - 1).mkString(" ")

        // Checks whether the last names are equal, but the starting portions are not, i.e. by verifying neither portion
        // is contained in the other.
        lastName1 == lastName2 && !(restName1.contains(restName2) || restName2.contains(restName1))
      } else
        false
    }
  }

}

/**
 * Anti-rule for testing whether the aliases of incident nodes have opposite gender.
 */
object GenderAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "gen"

  /**
   * Returns true if the given alias nodes have opposite gender, false otherwise.
   * @param edge Character edge with gendered alias ends
   * @param storyId Option of story Id (Not needed).
   * @return True if the given alias nodes have opposite gender, false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean = {
    val gender1 = edge.node1.alias match {
      case a: GenderedAlias => a.gender
      case a => throw new RuntimeException("Alias " + a + " must be gendered.")
    }

    val gender2 = edge.node2.alias match {
      case a: GenderedAlias => a.gender
      case a => throw new RuntimeException("Alias " + a + " must be gendered.")
    }

    (gender1 == FEMALE && gender2 == MALE) || (gender1 == MALE && gender2 == FEMALE)
  }

}

/**
 * Anti-rule for checking whether the aliases each have either 'Mrs.' and 'Miss', or 'Mrs.' and 'Ms.'.
 */
object FemaleSpousalTitleAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "fst"

  /**
   * Returns true if the spans denoted by the incident alias nodes each have either 'Mrs.' and 'Miss', or 'Mrs.' and
   * 'Ms.'.
   * @param edge Character edge.
   * @param storyId Option of story Id (Not needed).
   * @return True if the spans denoted by the incident alias nodes each have either 'Mrs.' and 'Miss', or 'Mrs.' and
   * 'Ms.', false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId] = None): Boolean = {
    val span1: String = edge.node1.getSpan
    val span2: String = edge.node2.getSpan

    NameTitles.hasTitle(span1, "Mrs.") && NameTitles.hasTitle(span2, "Ms.") ||
      NameTitles.hasTitle(span1, "Ms.") && NameTitles.hasTitle(span2, "Mrs.") ||
      NameTitles.hasTitle(span1, "Mrs.") && NameTitles.hasTitle(span2, "Miss") ||
      NameTitles.hasTitle(span1, "Miss") && NameTitles.hasTitle(span2, "Mrs.")
  }

}

/**
 * Checks whether the aliases appear in a conjunction construction in the given story's text.
 */
object ConjunctionAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "conj"

  /**
   * Returns true if the end aliases appear in a conjunction construction in the given story's text, false otherwise.
   * @param edge Character edge.
   * @param storyId Option of story Id (Required).
   * @return True if the end aliases appear in a conjunction construction in the given story's text, false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId]): Boolean = storyId match {
    case Some(sid) => ConjunctionAntiEdgeLoader.get(sid).exists(ae =>
      (edge.node1 == ae.node1 && edge.node2 == ae.node2) || (edge.node1 == ae.node2 && edge.node2 == ae.node1))
    case None => throw new RuntimeException("Story Id required.")
  }

}

/**
 * Checks whether the aliases appear in a speaker/speeched-mentioned construction in the given story's text.
 */
object DialogueAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "conj"

  /**
   * Returns true if the end aliases appear in a speaker/speeched-mentioned construction in the given story's text,
   * false otherwise.
   * @param edge Character edge.
   * @param storyId Option of story Id (Required).
   * @return True if the end aliases appear in a speaker/speeched-mentioned construction in the given story's text,
   *         false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId]): Boolean = storyId match {
    case Some(sid) => DialogueAntiEdgeLoader.get(sid).exists(ae =>
      (edge.node1 == ae.node1 && edge.node2 == ae.node2) || (edge.node1 == ae.node2 && edge.node2 == ae.node1))
    case None => throw new RuntimeException("Story Id required.")
  }

}

/**
 * Checks the aliases according to [[ConjunctionAntiRule]] or [[DialogueAntiRule]]
 */
object ConjunctionDialogueAntiRule extends CharacterAntiEdgeRule {

  /** Rule id. */
  override val id: String = "conjdiag"

  /**
   * Returns true if the end aliases satisfy either [[ConjunctionAntiRule]] or [[DialogueAntiRule]].
   * @param edge Character edge.
   * @param storyId Option of story Id (Required).
   * @return True if the end aliases satisfy either anti-rule, false otherwise.
   */
  override def apply(edge: CharacterEdge, storyId: Option[StoryId]): Boolean = storyId match {
    case Some(sid) => ConjunctionDialogueAntiEdgeLoader.get(sid).exists(ae =>
      (edge.node1 == ae.node1 && edge.node2 == ae.node2) || (edge.node1 == ae.node2 && edge.node2 == ae.node1))
    case None => throw new RuntimeException("Story Id required.")
  }

}

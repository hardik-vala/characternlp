package dictractor.extractors.proposed

import java.io.File

import altractor.AliasExtractor
import dict.{Alias, CharacterDictionary}
import dictractor.extractors.DictExtractor
import results.Results
import story.StoryId


/**
 * Proposed character dictionary extractor.
 * @param aliasExtractor Alias extractor.
 * @param nodeWeightAssigner Node weight assigner.
 * @param edgeWeightAssigner Edge weight assigner.
 * @param rules Character edge rules for determining edges.
 * @param antiRules Character anti-edge rules for determining anti-edges.
 * @param graphTransforms Character graph transforms.
 */
class ProposedExtractor(aliasExtractor: AliasExtractor,
                        nodeWeightAssigner: NodeWeightAssigner,
                        edgeWeightAssigner: EdgeWeightAssigner,
                        rules: Seq[CharacterEdgeRule],
                        antiRules: Seq[CharacterAntiEdgeRule],
                        graphTransforms: Seq[CharacterGraphTransform]) extends DictExtractor {

  /** Id of extractor for the purposes of naming relevent files. */
  val id: String = this.nodeWeightAssigner.id + "-" + this.edgeWeightAssigner.id + "-" +
    (this.rules.map(_.id) ++ this.antiRules.map(_.id) ++ this.graphTransforms.map(_.id) mkString "-")

  /** PROPOSED cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "proposed" + File.separator + this.id + ".csv"
  /** PROPOSED cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String =
    "proposed-wt-cnts" + File.separator +this.id + ".csv"
  /** PROPOSED cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "proposed" + File.separator + this.id
  /** PROPOSED cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "proposed-wt-cnts" + File.separator + this.id

  // Directory path with character graph .csv files.
  private val GRAPH_DIRPATH = Results.DIRPATH + File.separator + "graphs" + File.separator + this.id

  // Create the graph directory if it doesn't already exist.
  if (!new File(this.GRAPH_DIRPATH).exists())
    new File(this.GRAPH_DIRPATH).mkdirs()

  // Extract the aliases for the given story.
  private def getAliases(storyId: StoryId): Set[_ <: Alias] = this.aliasExtractor.extract(storyId)

  // Convert the given set of aliases to alias nodes.
  private def aliasesToNodes(aliases: Set[_ <: Alias]): Set[AliasNode] = aliases.map(new AliasNode(_))

  // Get the set of all possible character edges between the given set of alias nodes, i.e. those edges in a complete
  // graph of these nodes.
  private def getEdges(aliasNodes: Set[AliasNode]): Set[CharacterEdge] =
    aliasNodes
      .toList
      .combinations(2)
      .map(c => CharacterEdge(c.head, c.last))
      .toSet

  // Apply the given set of character graph edge rules to determine if the given edge satisfies at least one of them or
  // not.
  private def applyRules(storyId: StoryId)(rules: Seq[CharacterGraphEdgeRule])(edge: CharacterEdge): Boolean =
    rules.foldLeft(false)({ case (res: Boolean, r: CharacterGraphEdgeRule) => res || r.apply(edge, Some(storyId)) })

  // Generates the set of character anti-edges from the set of all possible edges.
  private def getCharacterAntiEdges(edges: Set[CharacterEdge], storyId: StoryId): Set[CharacterAntiEdge] =
    edges.filter(this.applyRules(storyId)(this.antiRules)).map(e => CharacterAntiEdge(e.node1, e.node2))

  // Gets the set of character edges from the set of all possible edges by applying the given rules and filtering out
  // edges that also belong to the given set of anti-edges.
  private def getCharacterEdges(edges: Set[CharacterEdge],
                                antiEdges: Set[CharacterAntiEdge],
                                storyId: StoryId): Set[CharacterEdge] =
    edges.filterNot(e => antiEdges.exists(_ == e)).filter(this.applyRules(storyId)(this.rules))

  /**
   * Extract the proposed character dictionary for the given story.
   * @param storyId Story id of story.
   * @return Extracted proposed character dictionary.
   */
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = {
    if (this.inCache(storyId, withCounts))
      this.fromCache(storyId, withCounts)
    else {
      // Extract the aliases in the story.
      val aliases: Set[_ <: Alias] = this.getAliases(storyId)
      // Convert the aliases to nodes.
      val nodes: Set[AliasNode] = this.aliasesToNodes(aliases)
      // Assign each alias node a weights.
      this.nodeWeightAssigner.assign(storyId, nodes)

      // Get all possible edges, i.e. those in the complete graph.
      val edges: Set[CharacterEdge] = this.getEdges(nodes)
      // Assign each possible edge a weight.
      this.edgeWeightAssigner.assign(storyId, edges)

      // Determine the character anti-edges.
      val characterAntiEdges: Set[CharacterAntiEdge] =
        this.getCharacterAntiEdges(edges, storyId)
      // Then the character edges.
      val characterEdges = this.getCharacterEdges(edges, characterAntiEdges, storyId)

      // Create the character graph and apply the graph transforms.
      val g = CharacterGraphTransform.apply(new CharacterGraph(nodes, characterEdges, characterAntiEdges),
        this.graphTransforms, Some(storyId), Some(this.nodeWeightAssigner))

      // Filepath to the saved graph output .csv file.
//        val graphFilepath = this.GRAPH_DIRPATH + File.separator +
//          (if (storyId.storySet == PRIDE_AND_PREJUDICE) "pride-and-prejudice.csv"
//          else new File(this.getCachedFilepath((storyId, false))).getName)

      // Save the graph to .csv.
//        g.toCSV(graphFilepath)

      // Generate the character dictionary from the characters determined by the character graph.
      val cd = new CharacterDictionary(storyId, g.toCharacters)
      // Save the character dictionary to cache.
      this.toCache((storyId, withCounts), cd)
      cd
    }
  }

}

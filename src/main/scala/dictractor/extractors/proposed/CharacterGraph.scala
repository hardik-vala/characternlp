package dictractor.extractors.proposed

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import dict.{GenderedAlias, Character, Alias}
import dictractor.coref.NormalizedCorefResoluter
import name.MALE
import story.StoryId
import text.{ORIG, TextManager}
import utils.Utils

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge


/**
 * For weighted graph entities (i.e. nodes and edges).
 */
trait Weighted {

  /** Weight. */
  protected var _w: Option[Double] = None

  /**
   * Weight getter.
   * @return Weight.
   */
  def weight: Option[Double] = this._w

  /**
   * Weight setter.
   * @param w Weight.
   */
  def weight_= (w: Double): Unit = this._w = Some(w)

}

/**
 * Graph node representing an alias.
 * @param alias Alias.
 */
case class AliasNode(alias: Alias) extends Weighted {

  /**
   * Alternate constructor using an alias span.
   * @param aliasSpan Alias span.
   * @return Alias node.
   */
  def this(aliasSpan: String) = this(Alias(aliasSpan, None))

  /**
   * Get the alias span.
   * @return Alias span.
   */
  def getSpan: String = this.alias.span

  /**
   * Checks equality with another node based on the alias span.
   * @param o Object.
   * @return True if the given object is an alias node with the same span.
   */
  override def equals(o: Any): Boolean = o match {
    case an: AliasNode => this.getSpan == an.getSpan
    case _ => false
  }

  override def hashCode: Int = this.alias.hashCode

  override def toString: String = this._w match {
    case Some(w) => "Node(" + this.getSpan + "," + "%.3f".format(w) + ")"
    case None => "Node(" + this.getSpan + ")"
  }

}

/**
 * Represents an undirected edge in the character graph.
 */
sealed trait CharacterGraphEdge extends Weighted {

  /** One alias node end of the edge. */
  val node1: AliasNode
  /** Other alias node end of the edge. */
  val node2: AliasNode

  /**
   * Span of first alias node.
   * @return Alias span of first alias node.
   */
  def node1Span: String = this.node1.getSpan

  /**
   * Span of second alias node.
   * @return Alias span of second alias node.
   */
  def node2Span: String = this.node2.getSpan
  
  /**
   * Checks equality with another edge based on the equality of the two end alias nodes.
   * @param o Object.
   * @return True if the object is a character graph edge and has the same ends.
   */
  override def equals(o: Any): Boolean = o match {
    case e: CharacterGraphEdge =>
      this.node1 == e.node1 && this.node2 == e.node2 || this.node1 == e.node2 && this.node2 == e.node1
    case _ => false
  }

  override def hashCode: Int = (this.node1, this.node2).hashCode

  /**
   * Checks if the given edge is incident on the given alias node.
   * @param a Alias node.
   * @return True if the given node is an end of the edge, false otherwise.
   */
  def isIncident(a: AliasNode): Boolean = this.node1 == a || this.node2 == a

}

/**
 * Represents an edge in the character graph denoting "charactership".
 * @param node1 One alias node end.
 * @param node2 Other alias node end.
 */
case class CharacterEdge(node1: AliasNode, node2: AliasNode) extends CharacterGraphEdge {

  override def toString: String = this._w match {
    case Some(w) => "Edge(" + this.node1.getSpan + "," + this.node2.getSpan + ",%.3f".format(w) + ")"
    case None => "Edge(" + this.node1.getSpan + "," + this.node2.getSpan + ")"
  }

}

/**
 * Represents an edge in the character graph denoteing "non-charactership".
 * @param node1 One alias node end.
 * @param node2 Other alias node end.
 */
case class CharacterAntiEdge(node1: AliasNode, node2: AliasNode) extends CharacterGraphEdge {

  override def toString: String = this._w match {
    case Some(w) => "AntiEdge(" + this.node1.getSpan + "," + this.node2.getSpan + ",%.3f".format(w) + ")"
    case None => "AntiEdge(" + this.node1.getSpan + "," + this.node2.getSpan + ")"
  }

}

/**
 * Node weight assigner.
 */
trait NodeWeightAssigner {

  /** Id. */
  val id: String

  /**
   * Assigns a weight to each of the alias nodes in the iterable for the given story.
   * @param storyId Story id of story.
   * @param nodes Iterable of character graph alias nodes.
   */
  def assign(storyId: StoryId, nodes: Iterable[AliasNode]): Unit

}

object CountNodeWeightAssigner extends NodeWeightAssigner {

  /** Id. */
  override val id: String = "c"

  /**
   * Assigns a weight to each of the alias nodes in the iterable according to the number of occurrences in the given
   * text.
   * @param text Text.
   * @param nodes Iterable of character graph alias nodes.
   */
  def assign(text: String, nodes: Iterable[AliasNode]): Unit = {
    val aliasSpanCounts: Map[String, Int] = Utils.phraseCounts(nodes.map(_.getSpan), text)
    nodes.foreach(n => n.weight = aliasSpanCounts.get(n.getSpan).get.toDouble)
  }

  /**
   * Assigns a weight to each of the alias nodes in the iterable according to the number of occurrences in the given
   * story's text.
   * @param storyId Story id of story.
   * @param nodes Iterable of character graph alias nodes.
   */
  override def assign(storyId: StoryId, nodes: Iterable[AliasNode]): Unit =
    this.assign(TextManager.getText(storyId, ORIG), nodes)

}

/**
 * Edge weight assigner.
 */
trait EdgeWeightAssigner {

  /** Id. */
  val id: String

  /**
   * Assigns a weight to each of the edges in the iterable for the given story.
   * @param storyId Story id of story.
   * @param edges Iterable of character graph edges.
   */
  def assign(storyId: StoryId, edges: Iterable[CharacterGraphEdge]): Unit

}

/**
 * Edge weight assigner based on coref. pairs.
 */
object CorefPairEdgeWeightAssigner extends EdgeWeightAssigner {

  /** Id. */
  override val id: String = "cp"

  // Assigns a weight to each edge based on the given coref. pairs.
  private def assign(corefPairs: Map[(String, String), Int], edges: Iterable[CharacterGraphEdge]): Unit =
    edges.foreach(e => e.weight = corefPairs.getOrElse((e.node1.getSpan, e.node2.getSpan), 0).toDouble)

  /**
   * Assigns a weight to each edge based on the co-occurrence of each of the aliases in coreference chains, i.e. coref.
   * pairs.
   * @param storyId Story id of story.
   * @param edges Iterable of character graph edges.
   */
  override def assign(storyId: StoryId, edges: Iterable[CharacterGraphEdge]): Unit =
    this.assign(new CorefPairGenerator(NormalizedCorefResoluter).gen(storyId), edges)

}

/**
 * Character graph, with aliases as nodes, edges denoting "charactership", and anti-edges "non-charactership".
 * @param nodes Alias nodes (weighted).
 * @param edges Character edges.
 * @param antiEdges Character anti-edges.
 */
class CharacterGraph(val nodes: Iterable[AliasNode],
                     val edges: Iterable[CharacterEdge],
                     val antiEdges: Iterable[CharacterAntiEdge]) {
  import CharacterGraph._

  // scalax representation of the graph.
  private val g: Graph[AliasNode, UnDiEdge] = Graph.from(nodes, edges.map(toUniDiEdge))

  // Node weights (Must be initialized for each of the nodes).
  private def nodeWeights: Map[AliasNode, Double] = this.nodes.map(n => n -> n.weight.get).toMap

  /**
   * Returns the node weight for the given alias node.
   * @param n Alias node.
   * @return Weight of the node as an option, None if it doesn't exist in the graph.
   */
  def getWeight(n: AliasNode): Option[Double] = this.nodeWeights.get(n)

  /**
   * Returns the immediate neighbours in the character graph for the given alias node.
   * @param a Alias node.
   * @return Set of neighbours of the given alias node.
   */
  def neighbours(a: AliasNode): Set[AliasNode] =
    this.g.get(a).diSuccessors.map(s => new AliasNode(s.alias))

  /**
   * Returns the degree of the given alias node.
   * @param n Alias node.
   * @return Degree.
   */
  def degree(n: AliasNode): Int = this.g.get(n).degree

  /**
   * Extracts the set of character represented by the given character graph, which consists of the connected components.
   * @return Set of characters.
   */
  def toCharacters: Set[Character] =
    this.g
    .componentTraverser()
    .map(c => new Character(c.nodes.map(_.alias)))
    .toSet

  /**
   * Outputs the given character graph to a .csv file located by the given filepath.
   * @param filepath Filepath of .csv file to save to.
   */
  def toCSV(filepath: String): Unit = {
    CSVWriter.open(new File(filepath)).writeAll(this.g.nodes.map(n => List(n.getSpan, "NODE")).toList ++
      this.edges
        .filterNot(e => this.g.find(toUniDiEdge(e)).isEmpty)
        .map(e => List(e.node1.getSpan, e.node2.getSpan, "EDGE"))
        .toList ++
      this.antiEdges
        .filterNot(ae => this.g.find(ae.node1).isEmpty || this.g.find(ae.node2).isEmpty)
        .map(ae => List(ae.node1.getSpan, ae.node2.getSpan, "ANTIEDGE")))
  }

}

object CharacterGraph {

  // Converts a character graph edge to a scalax undirected edge.
  private def toUniDiEdge(e: CharacterGraphEdge): UnDiEdge[AliasNode] = UnDiEdge(e.node1, e.node2)

  // Removes nodes with degree 0 and weight less than the given threshold, from the given graph.
//  private def pruneSingletons(g: Graph[AliasNode, UnDiEdge],
//                              nodeWeights: Map[AliasNode, Double],
//                              t: Int): Graph[AliasNode, UnDiEdge] =
//    g.nodes.map(n => new AliasNode(n.alias)).foldLeft(g)({
//      case (h, n) => if ((h.get(n).degree == 0) && (nodeWeights.get(n).get < t)) h - n else h
//    })

}

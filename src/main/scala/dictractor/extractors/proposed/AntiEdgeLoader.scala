package dictractor.extractors.proposed

import java.io.File

import dict.Alias
import story.{StoryOverseer, StoryId}
import story.StorySet._


/**
 * Loads stored custom anti-edges.
 */
abstract class AntiEdgeLoader {

  /** Directory name containing anti-edge files. */
  protected val DIR_NAME: String = "anti-edges"

  /** Name associated to anti-edge loader. */
  protected val name: String

  /** Map from story to corresponding anti-edges. */
  protected val m: scala.collection.mutable.Map[StoryId, Set[CharacterAntiEdge]] =
    scala.collection.mutable.Map()

  /**
   * Loads the anti-edges from the .tsv file located by the given path.
   * @param filepath Path to .tsv file storing anti-edges.
   * @return Set of anti-edges.
   */
  protected def load(filepath: String): Set[CharacterAntiEdge] = {
    // Indices in .tsv file.
    val aliasSpan1Index = 0
    val aliasSpan2Index = 1

    io.Source.fromFile(new File(filepath)).getLines().map(_.split("\t")).map(entries => {
      val alias1 = new Alias(entries(aliasSpan1Index))
      val alias2 = new Alias(entries(aliasSpan2Index))
      CharacterAntiEdge(AliasNode(alias1), AliasNode(alias2))
    }).toSet
  }

  /**
   * Get the anti-edges for the given story.
   * @param storyId Story id of story.
   * @return Set of anti-edges.
   */
  def get(storyId: StoryId): Set[CharacterAntiEdge] = {
    if (this.m.get(storyId).isDefined)
      this.m(storyId)
    else {
      val filepath = storyId match {
        case StoryId(MOONSTONE, None) => StoryOverseer.getPath(MOONSTONE) + File.separator +
          this.DIR_NAME + File.separator + this.name + ".tsv"
        case StoryId(PRIDE_AND_PREJUDICE, None) => StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator +
          this.DIR_NAME + File.separator + this.name + ".tsv"
        case StoryId(SHERLOCK_HOLMES, Some(storyName)) => StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator +
          this.DIR_NAME + File.separator + this.name + File.separator + storyName + ".tsv"
        case StoryId(SILVER_STANDARD, Some(storyName)) => StoryOverseer.getPath(SILVER_STANDARD) + File.separator +
          this.DIR_NAME + File.separator + this.name + File.separator + storyName + ".tsv"
        case StoryId(ELSON, Some(storyName)) => StoryOverseer.getPath(ELSON) + File.separator +
          this.DIR_NAME + File.separator + this.name + File.separator + storyName + ".tsv"
        case StoryId(PROJECT_GUTENBERG, Some(storyName)) => StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator +
          this.DIR_NAME + File.separator + this.name + File.separator + storyName + ".tsv"
      }

      this.m(storyId) = this.load(filepath)
      this.m(storyId)
    }
  }

}

/**
 * Conjunction anti-edge loader.
 */
object ConjunctionAntiEdgeLoader extends AntiEdgeLoader {

  /** Name associated to anti-edge loader. */
  override protected val name: String = "conjunction"

}

/**
 * Dialogue anti-edge loader.
 */
object DialogueAntiEdgeLoader extends AntiEdgeLoader {

  /** Name associated to anti-edge loader. */
  override protected val name: String = "dialogue"

}

/**
 * Conjunction and dialogue anti-edge loader for when the anti-edges are stored together.
 */
object ConjunctionDialogueAntiEdgeLoader extends AntiEdgeLoader {

  /** Name associated to anti-edge loader. */
  override protected val name: String = "conjunction-dialogue"

}

package network.alias.detection.detectors

import java.io.File

import network.alias.{Aliases, Alias}
import story.{StorySet, StoryNotFoundException, StoryOverseer, StoryId}
import story.StorySet._
import utils.Cache


/**
 * Alias detector
 */
trait AliasDetector extends Cache {

  // Filename for cached extracted aliases (Applies to single stories).
  protected val cacheAliasesFilename: String
  // Directory name for cached extracted aliases (Applies to story collections).
  protected val cacheAliasesDirname: String

  /**
   * Loads the list of extracted aliases stored in a cached .csv file specified by the given filepath.
   * @param filepath Filepath location of cached .csv file.
   * @return List of aliases.
   */
  override protected def fromCacheCSV(filepath: String): Any = Aliases.fromCSV(filepath)

  /**
   * Saves the given list of extracted aliases to a cached .csv file specified by the given filepath.
   * @param aliases List of extracted aliases.
   * @param filepath Filepath location to save to.
   */
  override protected def toCacheCSV(aliases: Any, filepath: String): Unit =
    Aliases.toCSV(aliases.asInstanceOf[List[Alias]], filepath)

  /**
   * Retrieve the filepath to the cached extracted aliases for the given story.
   * @param id Story id of story.
   * @return Filepath to cached extracted aliases.
   */
  override protected def getCachedFilepath(id: Any): String = id match {
    case StoryId(PRIDE_AND_PREJUDICE, None) =>
      val filepath = StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + "alias-detection" + File.separator +
        this.cacheAliasesFilename
      val dir = new File(new File(filepath).getParent)

      if (!dir.exists())
        dir.mkdirs()

      filepath
    case StoryId(MOONSTONE, None) |
         StoryId(SHERLOCK_HOLMES, Some(_)) |
         StoryId(SILVER_STANDARD, Some(_)) |
         StoryId(ELSON, Some(_)) |
         StoryId(PROJECT_GUTENBERG, Some(_)) |
         StoryId(PIPER, Some(_)) |
         StoryId(WILKENS, Some(_)) => throw new NotImplementedError
    case storyId: StoryId => throw new StoryNotFoundException(storyId)
    case _ => throw new IllegalArgumentException("Expected a story id.")
  }

  /**
   * Checks if the extracted aliases for the given story have been cached.
   * @param id Story id of story.
   * @return True if a cache copy of the extracted aliases exists, false otherwise.
   */
  override protected def inCache(id: Any): Boolean = new File(this.getCachedFilepath(id)).exists()

  /**
   * Retrieves the cached extracted aliases for the given story (Cache copy of the aliases must exist).
   * @param id Story id of the story.
   * @return Extracted aliases for the story.
   */
  override protected def fromCache(id: Any): List[Alias] =
    this.fromCacheCSV(this.getCachedFilepath(id)).asInstanceOf[List[Alias]]

  /**
   * Saves the extracted aliases for the given story to the cache.
   * @param id Story id of the story.
   * @param aliases Extracted aliases.
   */
  override protected def toCache(id: Any, aliases: Any): Unit = this.toCacheCSV(aliases, this.getCachedFilepath(id))

  /**
   * Clears the cache.
   */
  override def clearCache: Unit = {
    for (storySet <- StorySet.values) {
      storySet match {
        case PRIDE_AND_PREJUDICE => this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE))
        case _ => System.err.println("WARNING (Clearing cache): Unhandled story set, " + storySet + ".")
      }
    }
  }

  /**
   * Extracts the list of aliases for the given story.
   * @param storyId Story id of story.
   * @return List of extracted aliases.
   */
  def detect(storyId: StoryId): List[Alias]

}

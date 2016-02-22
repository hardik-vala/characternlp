package altractor

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import dict._
import story.StorySet
import story._
import story.StorySet._
import utils.Cache


trait AliasExtractorCache extends Cache {
 
  // Directory name with cached extracted aliases.
  private val aliasesDirname = "aliases"
  
  /** Filename for cached extracted aliases without counts (Applies to single stories). */
  protected val cacheAliasesFilename: String
  /** Directory name for cached extracted aliases without counts (Applies to story collections). */
  protected val cacheAliasesDirname: String

  /**
   * Loads extracted aliases as a set from a cached file.
   * @param filepath Filepath location of cached list of aliases.
   * @return Set of aliases.
   */
  override protected def fromCacheCSV(filepath: String): Any =
    io.Source.fromFile(new File(filepath)).getLines().map(new Alias(_)).toSet

  /**
   * Saves the given extracted aliases as a cached file specified by the given filepath.
   * @param aliases Extracted aliases to save.
   * @param filepath Filepath location to save to.
   */
  override protected def toCacheCSV(aliases: Any, filepath: String): Unit =
    CSVWriter.open(new File(filepath)).writeAll(aliases.asInstanceOf[Set[Alias]].map(_.span).toList.map(_::Nil))

  /**
   * Retrieve the filepath to the cached extracted aliases for the given story.
   * @param storyId Story id of story.
   * @return Filepath to cached extracted aliases.
   */
  override protected def getCachedFilepath(storyId: Any): String = storyId match {
    case StoryId(MOONSTONE, None) =>
      val dir = new File(StoryOverseer.getPath(MOONSTONE) + File.separator + this.aliasesDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + this.cacheAliasesFilename
    case StoryId(PRIDE_AND_PREJUDICE, None) =>
      val dir = new File(StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + this.aliasesDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + this.cacheAliasesFilename
    case StoryId(SHERLOCK_HOLMES, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + this.aliasesDirname +
        File.separator + this.cacheAliasesDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(SILVER_STANDARD, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(SILVER_STANDARD) + File.separator + this.aliasesDirname +
        File.separator + this.cacheAliasesDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(ELSON, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(ELSON) + File.separator + this.aliasesDirname +
        File.separator + this.cacheAliasesDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(PROJECT_GUTENBERG, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + this.aliasesDirname +
        File.separator + this.cacheAliasesDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case _: StoryId => throw new StoryNotFoundException(storyId.asInstanceOf[StoryId])
    case _ => throw new IllegalArgumentException("Expected a story id.")
  }

  /**
   * Checks if the extracted aliases for the given story has been cached.
   * @param storyId Story id of story.
   * @return True if a cache copy of the extracted alises exists, false otherwise.
   */
  override protected def inCache(storyId: Any): Boolean = new File(this.getCachedFilepath(storyId)).exists()

  /**
   * Retrieves the cached extracted aliases for the given story (Cache copy of the aliases must exist).
   * @param storyId Story id of story. 
   * @return Extracted aliases for the story as a set.
   */
  override protected def fromCache(storyId: Any): Any =
    this.fromCacheCSV(this.getCachedFilepath(storyId))

  /**
   * Saves the given extracted aliases to the cache.
   * @param storyId Story id of story.
   * @param aliases Set of extracted aliases.
   */
  override protected def toCache(storyId: Any, aliases: Any): Unit =
    this.toCacheCSV(aliases, this.getCachedFilepath(storyId))

  /**
   * Clears the cache.
   */
  override protected def clearCache: Unit = {
    for (storySet <- StorySet.values) {
      storySet match {
        case MOONSTONE => this.deleteFromCache(new StoryId(MOONSTONE))
        case PRIDE_AND_PREJUDICE => this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE))
        case SHERLOCK_HOLMES =>
          // Delete all cached dictionaries in cache directories.
          SherlockHolmes.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SherlockHolmes.ids.head)).getParent).delete()
        case SILVER_STANDARD =>
          // Delete all cached dictionaries in cache directories.
          SilverStandard.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SilverStandard.ids.head)).getParent).delete()
        case ELSON =>
          // Delete all cached dictionaries in cache directories.
          Elson.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(Elson.ids.head)).getParent).delete()
        case PROJECT_GUTENBERG =>
          // Delete all cached dictionaries in cache directories.
          ProjectGutenberg.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(ProjectGutenberg.ids.head)).getParent).delete()
        case _ => throw new RuntimeException("Unhandled story set.")
      }
    }
  }
}

/**
 * Alias extractor for a story.
 */
trait AliasExtractor extends AliasExtractorCache {

  // Checks if the alias refers to the narrator.
  protected def isNarrator(alias: Alias): Boolean = alias.span.toLowerCase.contains("narrator")

  /**
   * Extracts the set of aliases in a given story.
   * @param storyId Story id of story.
   * @return Set of aliases.
   */
  def extract(storyId: StoryId): Set[_ <: Alias]

}

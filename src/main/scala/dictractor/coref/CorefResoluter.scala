package dictractor.coref

import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import name.{UNKNOWN, MALE, FEMALE, Gender}
import story.StorySet
import story._
import story.StorySet._
import utils.Cache


case class AliasCorefMention(span: String, gender: Gender) {

  /**
   * Checks if the gender is female.
   * @return True if the gender is female, false otherwise.
   */
  def isFemale: Boolean = this.gender == FEMALE

  /**
   * Checks if the gender is male.
   * @return True if the gender is male, false otherwise.
   */
  def isMale: Boolean = this.gender == MALE

  /**
   * Checks if the gender is unknown.
   * @return True if the gender is unknown, false otherwise.
   */
  def hasUnknownGender: Boolean = this.gender == UNKNOWN

  override def toString: String = this.span + " [" + this.gender.name + "]"

}

/**
 * Cache for coreference resolution related entities.
 */
trait CorefCache extends Cache {

  private val corefCacheDirname: String = "coref"

  /** Cache filename for the coref. output of a single story. */
  protected val cacheStoryFilename: String
  /** Cache directory name for coref. output of the texts in a story collection. */
  protected val cacheStoryDirname: String

  /**
   * Loads the coref. output from the cached .csv file located by the given filepath.
   * @param filepath Filepath to the cached coref. output as a .csv file.
   * @return Coref. output as a list of chains, each of them as list of mentions.
   */
  override protected def fromCacheCSV(filepath: String): Any =
    CSVReader.open(new File(filepath)).all().map(_.map(s => {
      val ss = s.split(" \\[")
      val gender = ss(1).substring(0, ss(1).length - 1) match {
        case "FEMALE" => FEMALE
        case "MALE" => MALE
        case "UNKNOWN" => UNKNOWN
      }
      AliasCorefMention(ss(0), gender)
    }))

  /**
   * Saves the coref. output to the given filepath.
   * @param coref Coref. output as a list of mention lists.
   * @param filepath Filepath location to the cache .csv file.
   */
  override protected def toCacheCSV(coref: Any, filepath: String): Unit =
    CSVWriter.open(new File(filepath)).writeAll(coref.asInstanceOf[List[List[AliasCorefMention]]]
      .map(_.map(_.toString)))

  /**
   * Returns the filepath to the cached coref. output for a given story.
   * @param storyId Story id of story.
   * @return Filepath to cache .csv file of object.
   */
  override protected def getCachedFilepath(storyId: Any): String = storyId match {
    case StoryId(MOONSTONE, None) => StoryOverseer.getPath(MOONSTONE) + File.separator +
      this.corefCacheDirname + File.separator + this.cacheStoryFilename
    case StoryId(PRIDE_AND_PREJUDICE, None) => StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator +
      this.corefCacheDirname + File.separator + this.cacheStoryFilename
    case StoryId(SHERLOCK_HOLMES, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + this.corefCacheDirname +
        File.separator + this.cacheStoryDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(SILVER_STANDARD, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(SILVER_STANDARD) + File.separator + this.corefCacheDirname +
        File.separator + this.cacheStoryDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(ELSON, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(ELSON) + File.separator + this.corefCacheDirname +
        File.separator + this.cacheStoryDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(PROJECT_GUTENBERG, Some(name)) =>
      val dir = new File(StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + this.corefCacheDirname +
        File.separator + this.cacheStoryDirname)

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case id: StoryId => throw new StoryNotFoundException(id)
    case _ => throw new IllegalArgumentException("Expected story Id.")
  }

  /**
   * Clears the cache of coref. output corresponding to stories and story collections.
   */
  override protected def clearCache: Unit = {
    for (storySet <- StorySet.values) {
      storySet match {
        case MOONSTONE => this.deleteFromCache(new StoryId(MOONSTONE))
        case PRIDE_AND_PREJUDICE => this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE))
        case SHERLOCK_HOLMES =>
          // Delete all cached dictionaries in cache directory.
          SherlockHolmes.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SherlockHolmes.ids.head)).getParent).delete()
        case SILVER_STANDARD =>
          // Delete all cached dictionaries in cache directory.
          SilverStandard.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SilverStandard.ids.head)).getParent).delete()
        case ELSON =>
          // Delete all cached dictionaries in cache directory.
          Elson.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(Elson.ids.head)).getParent).delete()
        case PROJECT_GUTENBERG =>
          // Delete all cached dictionaries in cache directory.
          ProjectGutenberg.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(ProjectGutenberg.ids.head)).getParent).delete()
        case _ => throw new RuntimeException("Unhandled story set.")
      }
    }
  }

}

/**
 * Coreference resoluter.
 */
trait CorefResoluter extends CorefCache {

  /**
   * Resoluter id.
   */
  val id: String

  /**
   * Performs coreference resolution for the given story, outputting a list of chains, each represented as a list of
   * mention strings.
   * @param storyId Story id of story.
   * @return List of chains as lists of mentions.
   */
  def resolute(storyId: StoryId): List[List[AliasCorefMention]]

}

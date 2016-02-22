package dictractor.extractors

import java.io.File

import dict.{Alias, Character, CharacterDictionary, CharacterDictionaryManager}
import story.StorySet
import story.StorySet._
import story._
import utils.Cache


/**
 * Character dictionary extractor.
 */
trait DictExtractor extends Cache {

  // Filename for cached extracted dictionary without alias counts (Applies to single stories).
  protected val cacheDictFilename: String
  // Filename for cached extracted dictionary with alias counts (Applies to single stories).
  protected val cacheDictWithCountsFilename: String
  // Directory name for cached extracted dictionaries without alias counts (Applies to story collections).
  protected val cacheDictDirname: String
  // Directory name for cached extracted dictionaries with alias counts (Applies to story collections).
  protected val cacheDictWithCountsDirname: String
  
  /**
   * Loads an extracted character dictionary as a set of characters and the alias counts (if they're stored as well)
   * from a cached .csv file.
   * @param filepath Filepath location of cached dictionary.
   * @return A pair with the set of characters in the dictionary and the corresponding alias counts, if they're also
   *         stored, otherwise None for the alias counts.
   */
  override protected def fromCacheCSV(filepath: String): Any =
    CharacterDictionary.charactersFromCSV(filepath)

  /**
   * Saves the given extracted character dictionary as a cached .csv file specified by the given filepath.
   * @param cd Extracted character dictionary to save.
   * @param filepath Filepath location to save to.
   */
  override protected def toCacheCSV(cd: Any, filepath: String): Unit = {
    val cdAsDict = cd.asInstanceOf[CharacterDictionary]
    cdAsDict.toCSV(filepath, cdAsDict.hasAliasCounts)
  }

  /**
   * Retrieve the filepath to the cached extracted dictionary for the given story.
   * @param id A pair with the story id of the story and whether to get the filepath to the dictionary with alias
   *           counts. 
   * @return Filepath to cached extracted dictionary.
   */
  override protected def getCachedFilepath(id: Any): String = id match {
    case (StoryId(MOONSTONE, None), withCounts: Boolean) =>
      val filepath = CharacterDictionaryManager.getDir(StoryId(MOONSTONE, None)) + File.separator +
        (if (withCounts) this.cacheDictWithCountsFilename else this.cacheDictFilename)
      val dir = new File(new File(filepath).getParent)

      if (!dir.exists())
        dir.mkdirs()

      filepath
    case (StoryId(PRIDE_AND_PREJUDICE, None), withCounts: Boolean) =>
      val filepath = CharacterDictionaryManager.getDir(StoryId(PRIDE_AND_PREJUDICE, None)) + File.separator +
        (if (withCounts) this.cacheDictWithCountsFilename else this.cacheDictFilename)
      val dir = new File(new File(filepath).getParent)

      if (!dir.exists())
        dir.mkdirs()

      filepath
    case (StoryId(SHERLOCK_HOLMES, Some(name)), withCounts: Boolean) =>
      val storyId = StoryId(SHERLOCK_HOLMES, Some(name))
      val dir = new File(CharacterDictionaryManager.getDir(storyId) + File.separator +
        (if (withCounts) this.cacheDictWithCountsDirname else this.cacheDictDirname))

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case (StoryId(SILVER_STANDARD, Some(name)), withCounts: Boolean) =>
      val storyId = StoryId(SILVER_STANDARD, Some(name))
      val dir = new File(CharacterDictionaryManager.getDir(storyId) + File.separator +
        (if (withCounts) this.cacheDictWithCountsDirname else this.cacheDictDirname))

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case (StoryId(ELSON, Some(name)), withCounts: Boolean) =>
      val storyId = StoryId(ELSON, Some(name))
      val dir = new File(CharacterDictionaryManager.getDir(storyId) + File.separator +
        (if (withCounts) this.cacheDictWithCountsDirname else this.cacheDictDirname))

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case (StoryId(PROJECT_GUTENBERG, Some(name)), withCounts: Boolean) =>
      val storyId = StoryId(PROJECT_GUTENBERG, Some(name))
      val dir = new File(CharacterDictionaryManager.getDir(storyId) + File.separator +
        (if (withCounts) this.cacheDictWithCountsDirname else this.cacheDictDirname))

      // Create the directory if it doesn't already exist.
      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case (storyId: StoryId, _: Boolean) => throw new StoryNotFoundException(storyId)
    case _ => throw new IllegalArgumentException("Expected a pair with story id and boolean")
  }

  /**
   * Checks if the extracted dictionary for the given story has been cached.
   * @param id A pair with the story id of the story and whether the dictionary contains alias counts.  
   * @return True if a cache copy of the extracted dictionary exists, false otherwise.
   */
  override protected def inCache(id: Any): Boolean = new File(this.getCachedFilepath(id)).exists()

  /**
   * Retrieves the cached extracted dictionary for the given story (Cache copy of the dictionary must exist).
   * @param id A pair with the story id of the story and whether the dictionary contains alias counts.
   * @return Extracted dictionary for the story.
   */
  override protected def fromCache(id: Any): CharacterDictionary = {
    val (storyId: StoryId, withCounts: Boolean) = id.asInstanceOf[(StoryId, Boolean)]

    val (characters: Set[Character], optAliasCounts: Option[Map[Alias, Int]]) =
      this.fromCacheCSV(this.getCachedFilepath(id)).asInstanceOf[(Set[Character], Option[Map[Alias, Int]])]

    val cd = new CharacterDictionary(storyId, characters)
    if (withCounts)
      cd.setAliasCounts(optAliasCounts.get)

    cd
  }

  /**
   * Saves the given character dictionary to the cache.
   * @param id A pair with the story id of the story and whether the dictionary contains alias counts.
   * @param cd The character dictionary.
   */
  override protected def toCache(id: Any, cd: Any): Unit = {
    val withCounts: Boolean = id.asInstanceOf[(StoryId, Boolean)]._2

    if (withCounts)
      cd.asInstanceOf[CharacterDictionary].getAliasCounts

    this.toCacheCSV(cd, this.getCachedFilepath(id))
  }

  /**
   * Clears the cache.
   */
  override protected def clearCache: Unit = {
    for (storySet <- StorySet.values) {
      storySet match {
        case MOONSTONE =>
          this.deleteFromCache(new StoryId(MOONSTONE), false)
          this.deleteFromCache(new StoryId(MOONSTONE), true)
        case PRIDE_AND_PREJUDICE =>
          this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE), false)
          this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE), true)
        case SHERLOCK_HOLMES =>
          // Delete all cached dictionaries in cache directories.
          SherlockHolmes.ids.foreach(id => {
            this.deleteFromCache(id, false)
            this.deleteFromCache(id, true)
          })
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SherlockHolmes.ids.head, false)).getParent).delete()
          new File(new File(this.getCachedFilepath(SherlockHolmes.ids.head, true)).getParent).delete()
        case SILVER_STANDARD =>
          // Delete all cached dictionaries in cache directories.
          SilverStandard.ids.foreach(id => {
            this.deleteFromCache(id, false)
            this.deleteFromCache(id, true)
          })
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SilverStandard.ids.head, false)).getParent).delete()
          new File(new File(this.getCachedFilepath(SilverStandard.ids.head, true)).getParent).delete()
        case ELSON =>
          // Delete all cached dictionaries in cache directories.
          Elson.ids.foreach(id => {
            this.deleteFromCache(id, false)
            this.deleteFromCache(id, true)
          })
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(Elson.ids.head, false)).getParent).delete()
          new File(new File(this.getCachedFilepath(Elson.ids.head, true)).getParent).delete()
        case PROJECT_GUTENBERG =>
          // Delete all cached dictionaries in cache directories.
          ProjectGutenberg.ids.foreach(id => {
            this.deleteFromCache(id, false)
            this.deleteFromCache(id, true)
          })
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(ProjectGutenberg.ids.head, false)).getParent).delete()
          new File(new File(this.getCachedFilepath(ProjectGutenberg.ids.head, true)).getParent).delete()
        case _ => throw new RuntimeException("Unhandled story set.")
      }
    }
  }

  /**
   * Extract the character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether the output dictionary should have alias counts initialized or not.
   * @return Extracted character dictionary.
   */
  def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary

}

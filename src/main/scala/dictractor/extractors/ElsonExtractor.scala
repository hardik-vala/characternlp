package dictractor.extractors

import dict.CharacterDictionary
import story.StoryId
import story.StorySet._


/**
 * Character dictionary extractor based on Elson et. al., 2010.
 */
object ElsonExtractor extends DictExtractor {

  /** ELSON cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "elson.csv"
  /** ELSON cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String = "elson-wt-cnts.csv"
  /** ELSON cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "elson"
  /** ELSON cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "elson-wt-cnts"

  /**
   * Extract the ELSON character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether to include alias counts in the extracted dictionary.
   * @return Extracted character dictionary.
   */
  // TODO: Untokenize aliases in extracted dictionaries.
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = storyId match {
    case StoryId(MOONSTONE, None) => throw new NotImplementedError("ELSON character dictionary for The Moonstone DNE.")
    case StoryId(PRIDE_AND_PREJUDICE, None) | StoryId(SHERLOCK_HOLMES, Some(_)) =>
      if (withCounts)
        this.toCache((storyId, true), this.fromCache(storyId, false))

      this.fromCache(storyId, withCounts)
    case StoryId(SILVER_STANDARD, Some(name)) => throw new NotImplementedError("ELSON character dictionary for " +
      name + " in the Silver Standard set DNE.")
  }

}


package dictractor.extractors

import dict.CharacterDictionary
import story.StoryId
import story.StorySet._


/**
 * Character dictionary extractor implemented for the NAACL 2015 paper.
 */
object NAACLExtractor extends DictExtractor {

  /** NAACL cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "naacl.csv"
  /** NAACL cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String = "naacl-wt-cnts.csv"
  /** NAACL cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "naacl"
  /** NAACL cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "naacl-wt-cnts"

  /**
   * Extract the NAACL character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether to include alias counts in the extracted dictionary.
   * @return Extracted character dictionary.
   */
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = storyId match {
    case StoryId(MOONSTONE, None) => throw new NotImplementedError("NAACL character dictionary for The Moonstone DNE.")
    case StoryId(PRIDE_AND_PREJUDICE, None) | StoryId(SHERLOCK_HOLMES, Some(_)) =>
      if (withCounts)
        this.toCache((storyId, true), this.fromCache(storyId, false))

      this.fromCache(storyId, withCounts)
    case StoryId(SILVER_STANDARD, Some(name)) => throw new NotImplementedError("NAACL character dictionary for " +
      name + " in the Silver Standard set DNE.")
  }

}

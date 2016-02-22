package dictractor.extractors

import dict.CharacterDictionary
import story.{StoryNotFoundException, StoryId}


/**
 * Character dictionary extractor based on Ardanuy and Sporleder (2014).
 */
object ArdanuyExtractor extends DictExtractor {

  /** ARDANUY cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "ardanuy.csv"
  /** ARDANUY cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String = "ardanuy-wt-cnts.csv"
  /** ARDANUY cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "ardanuy"
  /** ARDANUY cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "ardunay-wt-cnts"

  /**
   * Extract the ARDANUY character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether to include alias counts in the extracted dictionary.
   * @return Extracted character dictionary.
   */
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = {
    if (this.inCache(storyId, withCounts))
      this.fromCache(storyId, withCounts)
    else if (withCounts && this.inCache(storyId, false)) {
      val cd: CharacterDictionary = this.fromCache(storyId, false)
      cd.getAliasCounts
      this.toCache((cd.storyId, true), cd)
      cd
    } else
      throw new StoryNotFoundException(storyId)
  }

}

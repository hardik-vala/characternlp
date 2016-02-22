package dictractor.extractors

import altractor.NPWithTitleExtractor
import dict.{Alias, Character, CharacterDictionary}
import story.StoryId


/**
 * Simply extracts the NP's.
 */
object NPDictExtractor extends DictExtractor {

  /** NP cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "np.csv"
  /** NP cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String = "np-wt-cnts.csv"
  /** NP cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "np"
  /** NP cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "np-wt-cnts"

  // Converts a set of NP's to a corresponding set of characters.
  private def NPsToCharacters(nps: Set[_ <: Alias]): Set[Character] = nps.map(np => Character(Set(np)))

  /**
   * Extract the NP's (with name titles) as a character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether the output dictionary should have alias counts initialized or not.
   * @return Extracted NP character dictionary.
   */
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = {
    if (this.inCache(storyId, withCounts))
      this.fromCache(storyId, withCounts)
    else if (withCounts && this.inCache(storyId, false)) {
      val cd: CharacterDictionary = this.fromCache(storyId, false)
      cd.getAliasCounts
      this.toCache((cd.storyId, true), cd)
      cd
    } else {
      val cd = new CharacterDictionary(storyId, NPsToCharacters(NPWithTitleExtractor.extract(storyId)))
      // Save the character dictionary to cache.
      this.toCache((cd.storyId, withCounts), cd)
      cd
    }
  }

}

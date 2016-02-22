package story

import java.io.File

import data.Data
import story.StorySet._


/**
 * Container class for a single story.
 */
object StoryOverseer {

  /**
   * Returns the path to the directory containing all story set data.
   * @param storySet Story set
   * @return Directory path as string.
   */
  def getPath(storySet: StorySet):String = storySet match {
    case ELSON => Data.DIRPATH + File.separator + "elson"
    case MOONSTONE => Data.DIRPATH + File.separator + "moonstone"
    case PIPER => Data.DIRPATH + File.separator + "piper"
    case PRIDE_AND_PREJUDICE => Data.DIRPATH + File.separator + "pride-and-prejudice"
    case PROJECT_GUTENBERG => Data.DIRPATH + File.separator + "project-gutenberg"
    case SCRIBNER => Data.DIRPATH + File.separator + "scribner"
    case SHERLOCK_HOLMES => Data.DIRPATH + File.separator + "sherlock-holmes"
    case SILVER_STANDARD => Data.DIRPATH + File.separator + "silver-standard"
    case WILKENS => Data.DIRPATH + File.separator + "wilkens"
  }

}

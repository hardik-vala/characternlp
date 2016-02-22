package story

import story.StorySet.StorySet


/**
 * Id for a given story. For story sets comprising of single story, only the story set should be specified. Otherwise,
 * both the story set, and the name of the story within the set are required.
 * @param storySet Story set.
 * @param storyName None for singular story sets, and the name of the story within the set for non-singular ones.
 */
case class StoryId(storySet: StorySet, storyName: Option[String]) {
  // Overloaded constructor for no story name.
  def this(storySet: StorySet) = this(storySet, None)
}

object StoryId {

  implicit def storyIdToString(storyId: StoryId): String =
    if (storyId.storyName.isDefined) storyId.storyName.get else storyId.storySet.toString

}

/**
 * Thrown to indicate the story corresponding to the given story Id cannot be found.
 * @param storyId Story Id.
 */
class StoryNotFoundException(storyId: StoryId) extends IllegalArgumentException("Unrecognized story Id, " + storyId +
  ".")

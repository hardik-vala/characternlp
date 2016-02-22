package network.alias

import java.io.File

import org.json4s.JsonAST.{JInt, JString, JArray}
import org.json4s._
import org.json4s.native.JsonMethods._
import story.{StoryNotFoundException, StoryOverseer, StoryId}
import story.StorySet._
import text.{STRIPPED, TextManager}


/**
 * Manages the ground truth aliases for stories.
 */
object AliasManager {

  // Paths to story alias files.
  private val paths: Map[StoryId, String] = Map(
    StoryId(PRIDE_AND_PREJUDICE, None) ->(StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator +
      "alias-detection" + File.separator + "aliases.json")
  )

  // Retrieves the list of aliases from the .json file specified by the given filepath, corresponding to the given text.
  private def getAliases(filepath: String, text: String): List[Alias] = {
    val json: JValue = parse(io.Source.fromFile(new File(filepath)).mkString)
    var aliases: List[Alias] = List()
    for {
      JArray(List(JString(id), JString(aliasType), JArray(List(JArray(List(JInt(startOffset), JInt(endOffset))))))) <-
      json
    } aliases :+= Alias(text.substring(startOffset.toInt, endOffset.toInt), startOffset.toInt, endOffset.toInt,
      Some(AliasType.withName(aliasType)))
    aliases
  }

  /**
   * Retrieves the list of ground truth aliases for the given story.
   * @param storyId Story id of story.
   * @return List of aliases.
   */
  def getAliases(storyId: StoryId): List[Alias] = storyId match {
    case StoryId(PRIDE_AND_PREJUDICE, None) =>
      this.getAliases(this.paths.get(storyId).get, TextManager.getText(storyId, STRIPPED))
    case StoryId(MOONSTONE, None) |
         StoryId(SHERLOCK_HOLMES, Some(_)) |
         StoryId(SILVER_STANDARD, Some(_)) |
         StoryId(ELSON, Some(_)) |
         StoryId(PROJECT_GUTENBERG, Some(_)) |
         StoryId(PIPER, Some(_)) |
         StoryId(WILKENS, Some(_)) => throw new NotImplementedError
    case _ => throw new StoryNotFoundException(storyId)
  }

}

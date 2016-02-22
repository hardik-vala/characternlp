import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import dict.CharacterDictionaryManager
import story.{StoryId, SherlockHolmes, PrideAndPrejudice}
import text.{TOK, TextManager}


object MinorCharactersSentences extends App {
  val MINOR_CHARACTER_THRESHOLD = 5

  def getMinorCharacterSentences(storyId: StoryId): List[(String, String)] = {
    val tokenizedText: String = TextManager.getText(storyId, TOK)
    val sentences: List[String] = tokenizedText.split(System.lineSeparator()).toList

    val minorAliases =
      CharacterDictionaryManager.getDictionary(storyId, false).asMinor(MINOR_CHARACTER_THRESHOLD).aliases

    minorAliases.flatMap(a =>
        sentences.filter(("(\\W|^)" + a.span + "\\W").r.findFirstIn(_).nonEmpty).map((a.span, _)))
  }

  def writeMinorCharacterSentencesToCSV(minorCharacterSentences: List[(String, String)], filepath: String): Unit =
    CSVWriter.open(new File(filepath)).writeAll(minorCharacterSentences.map(p => List(p._1, p._2)))

  val dir = new File("data/minor-character-sentences")
  if (!dir.exists())
    dir.mkdirs()

  writeMinorCharacterSentencesToCSV(getMinorCharacterSentences(PrideAndPrejudice.id), dir.getPath + File.separator +
    "pride-and-prejudice.csv")

  val shDir = new File(dir.getPath + File.separator + "sherlock-holmes")
  if (!shDir.exists())
    shDir.mkdirs()

  SherlockHolmes.ids.foreach(id => {
    writeMinorCharacterSentencesToCSV(getMinorCharacterSentences(id), shDir.getPath + File.separator +
      id.storyName.get + ".csv")
  })

}

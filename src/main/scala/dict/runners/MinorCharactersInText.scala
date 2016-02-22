import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import dict.CharacterDictionaryManager
import story.{SherlockHolmes, PrideAndPrejudice}
import text.{ORIG, TextManager}

/**
 *
 */
object MinorCharactersInText extends App {
  val MINOR_CHARACTER_THRESHOLD = 5
  val CONTEXT_WINDOW_SIZE = 300

  val prideAndPrejudiceText = TextManager.getText(PrideAndPrejudice.id, ORIG).replaceAll("\\s+", " ")

  val prideAndPrejudiceMinorAliases =
    CharacterDictionaryManager.getDictionary(PrideAndPrejudice.id, false).asMinor(MINOR_CHARACTER_THRESHOLD).aliases

  var contextMap: Map[String, List[(String, String, String)]] = Map()
  var t: String = prideAndPrejudiceText

  prideAndPrejudiceMinorAliases.map(_.span).sortBy(-_.length).foreach(s => {
    contextMap += s -> ("\\W" + s + "\\W").r.findAllMatchIn(t).map(m => {
      val preContextStartIndex = if (m.start - CONTEXT_WINDOW_SIZE < 0) 0 else m.start - CONTEXT_WINDOW_SIZE
      val postContextEndIndex =
        if (m.end + CONTEXT_WINDOW_SIZE > prideAndPrejudiceText.length) prideAndPrejudiceText.length
        else m.end + CONTEXT_WINDOW_SIZE

      val preContext = prideAndPrejudiceText.substring(preContextStartIndex, m.start)
      val postContext = prideAndPrejudiceText.substring(m.end, postContextEndIndex)

      (preContext, s, postContext)
    }).toList

    var replacementToken = ""
    var i = 0
    for (i <- 1 to s.length)
      replacementToken += "="

    t = ("\\W" + s + "\\W").r.replaceAllIn(t, " " + replacementToken + " ")
  })

  CSVWriter.open(new File("pride-and-prejudice.csv"))
    .writeAll(contextMap.values.flatten.toList.sortBy(_._2).map(triple => List(triple._1, triple._2, triple._3)))

  SherlockHolmes.ids.foreach(id => {
    val storyText = TextManager.getText(id, ORIG).replaceAll("\\s+", " ")

    val minorAliases =
      CharacterDictionaryManager.getDictionary(id, false).asMinor(MINOR_CHARACTER_THRESHOLD).aliases

    var contextMap: Map[String, List[(String, String, String)]] = Map()
    var t: String = storyText

    minorAliases.map(_.span).sortBy(-_.length).foreach(s => {
      contextMap += s -> ("\\W" + s + "\\W").r.findAllMatchIn(t).map(m => {
        val preContextStartIndex = if (m.start - CONTEXT_WINDOW_SIZE < 0) 0 else m.start - CONTEXT_WINDOW_SIZE
        val postContextEndIndex =
          if (m.end + CONTEXT_WINDOW_SIZE > storyText.length) storyText.length
          else m.end + CONTEXT_WINDOW_SIZE

        val preContext = storyText.substring(preContextStartIndex, m.start)
        val postContext = storyText.substring(m.end, postContextEndIndex)

        (preContext, s, postContext)
      }).toList

      var replacementToken = ""
      var i = 0
      for (i <- 1 to s.length)
        replacementToken += "="

      t = ("\\W" + s + "\\W").r.replaceAllIn(t, " " + replacementToken + " ")
    })

    CSVWriter.open(new File("minor-character-contexts/" + id.storyName.get + ".csv"))
      .writeAll(contextMap.values.filter(_.nonEmpty).flatten.toList.sortBy(_._2).map(triple =>
        List(triple._1, triple._2, triple._3)))
  })

}

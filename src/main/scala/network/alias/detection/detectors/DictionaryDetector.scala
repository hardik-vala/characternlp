package network.alias.detection.detectors

import java.io.File
import java.util.Properties

import altractor.NPPlusTitledExtractor
import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, PartOfSpeechAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import lang.{Pronouns, AnimateNouns}
import network.alias.detection.AliasDetectionEvaluator
import network.alias.{AliasManager, Alias}
import story.{PrideAndPrejudice, StoryId}
import text.{TextManager, STRIPPED}


/**
 * Dictionary-based alias detector.
 */
class DictionaryDetector(dictionary: Iterable[String]) extends AliasDetector {
  import DictionaryDetector._

  /** Dictionary cached aliases filename (for single stories) stored in standard .csv format. */
  override protected val cacheAliasesFilename: String = "dict.csv"
  /** Dictionary cached aliases directory name for story collections with aliases stored in standard .csv format. */
  override protected val cacheAliasesDirname: String = "dict"

  /**
   * Extracts the aliases in the given text according to the dictionary.
   * @param text Text to extract from.
   * @return List of aliases.
   */
  def detect(text: String): List[Alias] = DictionaryDetector.detect(this.dictionary, text)

  /**
   * Extracts the list of aliases according to the dictionary for the given story.
   * @param storyId Story id of story.
   * @return List of extracted aliases.
   */
  override def detect(storyId: StoryId): List[Alias] = {
    val cacheFile = new File(this.getCachedFilepath(storyId))

    if (cacheFile.exists())
      this.fromCacheCSV(cacheFile.getPath).asInstanceOf[List[Alias]]
    else {
      val aliases = this.detect(TextManager.getText(storyId, STRIPPED))
      this.toCacheCSV(aliases, cacheFile.getPath)
      aliases
    }
  }

}

object DictionaryDetector {

  /**
   * Extracts the aliases in the given text according to the given dictionary.
   * @param text Text to extract from.
   * @return List of aliases.
   */
  def detect(dictionary: Iterable[String], text: String): List[Alias] = {
    def dummyToken(length: Int): String = (0 until length).map(_ => "$").mkString

    def replaceWithDummyToken(t: String, startIndex: Int, endIndex: Int) =
      t.substring(0, startIndex) + dummyToken(endIndex - startIndex) + t.substring(endIndex, t.length)

    var t = text

    dictionary.toList.sortBy(-_.length).flatMap(d => {
      val regexD = d.replaceAll("\\s+", "\\\\s+")
      var aliases: List[Alias] =
        ("\\W" + regexD + "\\W").r.findAllIn(t).matchData.map(m => {
          val startOffset = m.start + 1
          val endOffset = m.end - 1
          t = replaceWithDummyToken(t, startOffset, endOffset)
          new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
        }).toList

      aliases ++= ("^" + regexD + "\\W").r.findAllIn(t).matchData.map(m => {
        val startOffset = m.start
        val endOffset = m.end - 1
        t = replaceWithDummyToken(t, startOffset, endOffset)
        new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
      }).toList

      aliases ++= ("\\W" + regexD + "$").r.findAllIn(t).matchData.map(m => {
        val startOffset = m.start + 1
        val endOffset = m.end
        t = replaceWithDummyToken(t, startOffset, endOffset)
        new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
      }).toList

      aliases ++= ("^" + regexD + "$").r.findAllIn(t).matchData.map(m => {
        val startOffset = m.start
        val endOffset = m.end
        t = replaceWithDummyToken(t, startOffset, endOffset)
        new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
      }).toList

      if (d(0).isLower) {
        val upperD = d(0).toUpper + (if (d.length > 1) d.substring(1, d.length) else "")
        val upperRegexD = upperD.replaceAll("\\s+", "\\s+")
        aliases ++=
          ("[\\.\\?!\"]\\W" + upperD + "\\W").r.findAllIn(t).matchData.map(m => {
            val startOffset = m.start + 2
            val endOffset = m.end - 1
            t = replaceWithDummyToken(t, startOffset, endOffset)
            new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
          }).toList

        aliases ++=
          ("\"" + upperD + "\\W").r.findAllIn(t).matchData.map(m => {
            val startOffset = m.start + 1
            val endOffset = m.end - 1
            t = replaceWithDummyToken(t, startOffset, endOffset)
            new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
          }).toList

        aliases ++=
          ("^" + upperD + "\\W").r.findAllIn(t).matchData.map(m => {
            val startOffset = m.start
            val endOffset = m.end - 1
            t = replaceWithDummyToken(t, startOffset, endOffset)
            new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
          }).toList

        aliases ++=
          ("[\\.\\?!\"]\\W" + upperD + "$").r.findAllIn(t).matchData.map(m => {
            val startOffset = m.start + 2
            val endOffset = m.end
            t = replaceWithDummyToken(t, startOffset, endOffset)
            new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
          }).toList

        aliases ++= ("^" + upperD + "$").r.findAllIn(t).matchData.map(m => {
          val startOffset = m.start
          val endOffset = m.end
          t = replaceWithDummyToken(t, startOffset, endOffset)
          new Alias(text.substring(startOffset, endOffset), startOffset, endOffset)
        }).toList
      }

      aliases
    }).sorted
  }

}

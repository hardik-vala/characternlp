package network.alias.detection.detectors

import java.io.File

import altractor.NPPlusTitledExtractor
import lang.{Pronouns, AnimateNouns}
import name.{Name, NameTitles}
import network.alias.Alias
import story.{PrideAndPrejudice, StoryId}
import text.{STRIPPED, TextManager}

import scala.util.matching.Regex


/**
 * Refines the output aliases detected by [[network.alias.detection.detectors.NormalizingCorefDetector]].
 */
object RefinedCorefDetector extends AliasDetector {

  /** Refined coref. cached aliases filename (for single stories) stored in standard .csv format. */
  override protected val cacheAliasesFilename: String = "coref-refined.csv"
  /** Refined coref. cached aliases directory name for story collections with aliases stored in standard .csv format. */
  override protected val cacheAliasesDirname: String = "coref-refined"

  // If the span of the alias is missing the preceding name title in the given text, then a new alias is returned with
  // the name title included, otherwise the original alias is returned.
  private def extractWithTitle(text: String)(a: Alias): Alias = {
    if (Name.isName(a.span) && !NameTitles.hasTitle(a.span)) {
      // Text before the given alias.
      val beforeText: String = text.substring(0, a.startOffset)

      // Finds the preceding title from the list of titles in the text preceding the given alias, if such a title exists
      // at all, returning the regex match.
      def findTitle(titles: List[String], titleMatch: Option[Regex.Match]): Option[Regex.Match] =
        (titles, titleMatch) match {
          case (_, Some(m)) => Some(m)
          case (Nil, _) => None
          case (h :: t, _) => findTitle(t, (h + "\\s$").r.findFirstMatchIn(beforeText))
        }

      val titleMatch = findTitle(NameTitles.get.toList, None)
      if (titleMatch.isEmpty)
        a
      else {
        val newStartOffset = titleMatch.get.start
        val endOffset = a.endOffset
        Alias(text.substring(newStartOffset, endOffset), newStartOffset, endOffset, a.aliasType)
      }
    } else
      a
  }

  // Remove aliases whose offsets are strictly contained in the offsets of another.
  private def removeSubsumedAliases(aliases: List[Alias]): List[Alias] = {
    // Checks whether the first alias is strictly subsumed by the other.
    def isSubsumed(a1: Alias, a2: Alias): Boolean =
      (a2.startOffset <= a1.startOffset && a1.endOffset < a2.endOffset) ||
        (a2.startOffset < a1.startOffset && a1.endOffset <= a2.endOffset)

    aliases.filterNot(a => aliases.exists(b => isSubsumed(a, b)))
  }

  /**
   * Extracts the list of aliases for the given story.
   * @param storyId Story id of story.
   * @return List of extracted aliases.
   */
  // TODO
  override def detect(storyId: StoryId): List[Alias] = {
    val cacheFile = new File(this.getCachedFilepath(storyId))

    if (cacheFile.exists()) {
      var aliases = this.fromCacheCSV(cacheFile.getPath).asInstanceOf[List[Alias]]
      // TODO
      aliases = (aliases.toSet |
        network.alias.AliasManager.getAliases(story.PrideAndPrejudice.id)
          .filter(a => a.span.toLowerCase == "her" | a.span == "Lucases"| a.span == "Gouldings" | a.span == "Bingleys" | a.span == "Collinses" | a.span == "Phillipses").toSet).toList
      val npPlusTitledAliases = NPPlusTitledExtractor.extract(PrideAndPrejudice.id).map(_.span)
      val pronouns = Pronouns.get.map(p => if (p == "I") p else p.toLowerCase)
      val dictionaryAliases = new DictionaryDetector(npPlusTitledAliases ++
        AnimateNouns.animateNouns ++ pronouns).detect(PrideAndPrejudice.id)
      aliases = (aliases.toSet | dictionaryAliases
        .filter(a => a.span.toLowerCase == "former" | a.span.toLowerCase == "latter" | a.span.toLowerCase == "who")
        .toSet).toList
      val confidentPronouns = Set("I", "Me", "He", "She", "Him", "You", "Himself", "Herself", "Myself", "Yourself",
        "Whom", "Us", "We", "Themselves", "Ourselves", "Yourselves", "Each other", "One another", "Who", "Those")
        .map(_.toLowerCase)
      aliases = (aliases.toSet | dictionaryAliases.filter(a => confidentPronouns.contains(a.span.toLowerCase)).toSet).toList
      aliases = (aliases.toSet | dictionaryAliases.filter(a => npPlusTitledAliases.contains(a.span)).toSet).toList
//      aliases = (aliases.toSet | dictionaryAliases.filter(a => pronouns.contains(a.span.toLowerCase)).toSet).toList
//      aliases = (aliases.toSet | dictionaryAliases.filter(a => AnimateNouns.animateNouns.contains(a.span.toLowerCase)).toSet).toList
      aliases
    }
    else {
      val strippedText = TextManager.getText(storyId, STRIPPED)
      val extractWithTitleFromStrippedText: (Alias) => Alias = extractWithTitle (strippedText)

      var aliases = NormalizingCorefDetector.detect(storyId).map(extractWithTitleFromStrippedText)
      aliases = (aliases.toSet |
        DictionaryDetector.detect(NPPlusTitledExtractor.extract(storyId).map(_.span), strippedText).toSet).toList
      aliases = this.removeSubsumedAliases(aliases)
      this.toCacheCSV(aliases, cacheFile.getPath)
      aliases
    }
  }

}

object RefinedCorefDetectorTest extends App {
//  val t = text.TextManager.getText(story.PrideAndPrejudice.id, text.STRIPPED)
//  val aliases = RefinedCorefDetector.detect(story.PrideAndPrejudice.id)

  val trueAliases = network.alias.AliasManager.getAliases(story.PrideAndPrejudice.id)



//  val precision = network.alias.detection.AliasDetectionEvaluator.precision(aliases, trueAliases)
//  val recall = network.alias.detection.AliasDetectionEvaluator.recall(aliases, trueAliases)
//
//  println(precision)
//  println(recall)
//  println(stats.Metrics.f1(precision, recall))

//  println()

//  network.alias.detection.AliasDetectionEvaluator.getFNs(aliases, trueAliases).foreach(println)
//  network.alias.detection.AliasDetectionEvaluator
//    .getFPs(aliases, trueAliases)
//    .take(10)
//    .map(a => t.substring(a.startOffset - 50, a.endOffset + 50))
//    .foreach(println)

}

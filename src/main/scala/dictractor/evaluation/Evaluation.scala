package dictractor.evaluation

import java.io.File
import java.sql.Timestamp

import com.github.tototoshi.csv.CSVWriter
import dict.{CharacterDictionary, CharacterDictionaryManager}
import dictractor.extractors.DictExtractor
import results.Results
import stats.Metrics
import stats.Metrics._
import story.{SilverStandard, StoryId, SherlockHolmes, PrideAndPrejudice}

import scala.collection.immutable.ListMap


/**
 * Evaluation for a number of character dictionary extractors.
 * @param dictExtractors Character dictionary extractors as a map from id to extractor.
 */
class Evaluation(dictExtractors: ListMap[String, DictExtractor]) {
  import Evaluation._

  /**
   * Outputs the evaluation results for the dictionary extractors against the gold dictionaries to a .csv file located
   * by the given filepath.
   * @param filepath Filepath location of output .csv file.
   */
  def evaluateToCSV(filepath: String): Unit = {
    var results: List[List[String]] = List(
      List(
        "Dict. Extractor",
        "# (MS)",
        "# Diff. (MS)",
        "P (MS)",
        "R (MS)",
        "F (MS)",
        "# (Major) (MS)",
        "# Diff. (Major) (MS)",
        "P (Major) (MS)",
        "R (Major) (MS)",
        "F (Major) (MS)",
        "Dict. Extractor",
        "# (PP)",
        "# Diff. (PP)",
        "P (PP)",
        "R (PP)",
        "F (PP)",
        "# (Major) (PP)",
        "# Diff. (Major) (PP)",
        "P (Major) (PP)",
        "R (Major) (PP)",
        "F (Major) (PP)",
        "Avg. # (SH)",
        "Std. Dev. # (SH)",
        "Avg. # Diff. (SH)",
        "Std. Dev. # Diff. (SH)",
        "Avg. P (SH)",
        "Std. Dev. P (SH)",
        "Avg. R (SH)",
        "Std. Dev. R (SH)",
        "Avg. F (SH)",
        "Std. Dev. F (SH)",
        "Avg. # (Major) (SH)",
        "Std. Dev. # (Major) (SH)",
        "Avg. # Diff. (Major) (SH)",
        "Std. Dev. # Diff. (Major) (SH)",
        "Avg. P (Major) (SH)",
        "Std. Dev. P (Major) (SH)",
        "Avg. R (Major) (SH)",
        "Std. Dev. R (Major) (SH)",
        "Avg. F (Major) (SH)",
        "Std. Dev. F (Major) (SH)",
        "Avg. # (SS)",
        "Std. Dev. # (SS)",
        "Avg. # Diff. (SS)",
        "Std. Dev. # Diff. (SS)",
        "Avg. R (SS)",
        "Std. Dev. R (SS)",
        "Avg. # (Major) (SS)",
        "Std. Dev. # (Major) (SS)",
        "Avg. # Diff. (Major) (SS)",
        "Std. Dev. # Diff. (Major) (SS)",
        "Avg. R (Major) (SS)",
        "Std. Dev. R (Major) (SS)"
      ) ++ SherlockHolmes.ids.flatMap(id => {
        val name = id.storyName.get
        List(
          s"# (SH, $name)",
          s"# Diff. (SH, $name)",
          s"P (SH, $name)",
          s"R (SH, $name)",
          s"F (SH, $name)",
          s"# (Major) (SH, $name)",
          s"# Diff. (Major) (SH, $name)",
          s"P (Major) (SH, $name)",
          s"R (Major) (SH, $name)",
          s"F (Major) (SH, $name)"
        )
      }) ++ SilverStandard.ids.flatMap(id => {
        val name = id.storyName.get
        List(
          s"# (SS, $name)",
          s"# Diff. (SS, $name)",
          s"R (SS, $name)",
          s"# (Major) (SS, $name)",
          s"# Diff. (Major) (SS, $name)",
          s"R (Major) (SS, $name)"
        )
      })
    )

    val msGoldDict = CharacterDictionaryManager.getDictionary(new StoryId(story.StorySet.MOONSTONE), annotated = false)
    val ppGoldDict = CharacterDictionaryManager.getDictionary(PrideAndPrejudice.id, annotated = false)
    val shGoldDicts = SherlockHolmes.ids.map(id =>
      id ->CharacterDictionaryManager.getDictionary(id, annotated = false)).toMap
    val ssGoldDicts = SilverStandard.ids.map(id =>
      id ->CharacterDictionaryManager.getDictionary(id, annotated = false)).toMap

    this.dictExtractors.foreach({ case (id: String, de: DictExtractor) =>
      val msEval = evaluateDictExtractor(de, msGoldDict)
      val ppEval = evaluateDictExtractor(de, ppGoldDict)
      val shEval = evaluateDictExtractor(de, shGoldDicts)
      val ssEval = evaluateDictExtractor(de, ssGoldDicts)

      results :+=
        List(
          id,
          "%d".format(msEval.get("#").get.toInt),
          "%.4f".format(msEval.get("# Diff.").get),
          "%.4f".format(msEval.get("P").get),
          "%.4f".format(msEval.get("R").get),
          "%.4f".format(msEval.get("F").get),
          "%d".format(msEval.get("# (Major)").get.toInt),
          "%.4f".format(msEval.get("# Diff. (Major)").get),
          "%.4f".format(msEval.get("P (Major)").get),
          "%.4f".format(msEval.get("R (Major)").get),
          "%.4f".format(msEval.get("F (Major)").get),
          "%d".format(ppEval.get("#").get.toInt),
          "%.4f".format(ppEval.get("# Diff.").get),
          "%.4f".format(ppEval.get("P").get),
          "%.4f".format(ppEval.get("R").get),
          "%.4f".format(ppEval.get("F").get),
          "%d".format(ppEval.get("# (Major)").get.toInt),
          "%.4f".format(ppEval.get("# Diff. (Major)").get),
          "%.4f".format(ppEval.get("P (Major)").get),
          "%.4f".format(ppEval.get("R (Major)").get),
          "%.4f".format(ppEval.get("F (Major)").get),
          "%.4f".format(shEval.get("Avg. #").get),
          "%.4f".format(shEval.get("Std. Dev. #").get),
          "%.4f".format(shEval.get("Avg. # Diff.").get),
          "%.4f".format(shEval.get("Std. Dev. # Diff.").get),
          "%.4f".format(shEval.get("Avg. P").get),
          "%.4f".format(shEval.get("Std. Dev. P").get),
          "%.4f".format(shEval.get("Avg. R").get),
          "%.4f".format(shEval.get("Std. Dev. R").get),
          "%.4f".format(shEval.get("Avg. F").get),
          "%.4f".format(shEval.get("Std. Dev. F").get),
          "%.4f".format(shEval.get("Avg. # (Major)").get),
          "%.4f".format(shEval.get("Std. Dev. # (Major)").get),
          "%.4f".format(shEval.get("Avg. # Diff. (Major)").get),
          "%.4f".format(shEval.get("Std. Dev. # Diff. (Major)").get),
          "%.4f".format(shEval.get("Avg. P (Major)").get),
          "%.4f".format(shEval.get("Std. Dev. P (Major)").get),
          "%.4f".format(shEval.get("Avg. R (Major)").get),
          "%.4f".format(shEval.get("Std. Dev. R (Major)").get),
          "%.4f".format(shEval.get("Avg. F (Major)").get),
          "%.4f".format(shEval.get("Std. Dev. F (Major)").get),
          "%.4f".format(ssEval.get("Avg. #").get),
          "%.4f".format(ssEval.get("Std. Dev. #").get),
          "%.4f".format(ssEval.get("Avg. # Diff.").get),
          "%.4f".format(ssEval.get("Std. Dev. # Diff.").get),
          "%.4f".format(ssEval.get("Avg. R").get),
          "%.4f".format(ssEval.get("Std. Dev. R").get),
          "%.4f".format(ssEval.get("Avg. # (Major)").get),
          "%.4f".format(ssEval.get("Std. Dev. # (Major)").get),
          "%.4f".format(ssEval.get("Avg. # Diff. (Major)").get),
          "%.4f".format(ssEval.get("Std. Dev. # Diff. (Major)").get),
          "%.4f".format(ssEval.get("Avg. R (Major)").get),
          "%.4f".format(ssEval.get("Std. Dev. R (Major)").get)
        ) ++ SherlockHolmes.ids.flatMap(id => {
          val eval = evaluateDictExtractor(de, shGoldDicts.get(id).get)
          List(
            "%d".format(eval.get("#").get.toInt),
            "%.4f".format(eval.get("# Diff.").get),
            "%.4f".format(eval.get("P").get),
            "%.4f".format(eval.get("R").get),
            "%.4f".format(eval.get("F").get),
            "%d".format(eval.get("# (Major)").get.toInt),
            "%.4f".format(eval.get("# Diff. (Major)").get),
            "%.4f".format(eval.get("P (Major)").get),
            "%.4f".format(eval.get("R (Major)").get),
            "%.4f".format(eval.get("F (Major)").get)
          )
        }) ++ SilverStandard.ids.flatMap(id => {
          val eval = evaluateDictExtractor(de, ssGoldDicts.get(id).get)
          List(
            "%d".format(eval.get("#").get.toInt),
            "%.4f".format(eval.get("# Diff.").get),
            "%.4f".format(eval.get("R").get),
            "%d".format(eval.get("# (Major)").get.toInt),
            "%.4f".format(eval.get("# Diff. (Major)").get),
            "%.4f".format(eval.get("R (Major)").get)
          )
        })
    })

    val writer = CSVWriter.open(new File(filepath))
    writer.writeAll(results.transpose)
    writer.close()
  }

  /**
   * Outputs the evaluation results for the dictionary extractors against the gold dictionaries to a .csv file located
   * in the results directory with a timestamped filename
   */
  def evaluateToCSV(): Unit = {
    val timeString = new Timestamp(System.currentTimeMillis()).toString
    this.evaluateToCSV(Results.DIRPATH + File.separator + timeString.substring(0, timeString.length - 4) + ".csv")
  }


}

object Evaluation {
  import CharacterDictionaryEvaluator._

  private implicit val t: Int = 5

  // Evaluates the given dictionary extractor against a gold dictionary, returning a map of the results.
  private def evaluateDictExtractor(dictExtractor: DictExtractor,
                                    goldDictionary: CharacterDictionary)(implicit t: Int): Map[String, Double] = {
    val extractedDictionary: CharacterDictionary = dictExtractor.extract(goldDictionary.storyId, withCounts = true)

    val goldMajorDictionary: CharacterDictionary = goldDictionary.asMajor(t)
    val extractedMajorDictionary: CharacterDictionary = extractedDictionary.asMajor(t)

    val precision: Double = CharacterDictionaryEvaluator.precision(extractedDictionary, goldDictionary)
    val recall: Double = CharacterDictionaryEvaluator.recall(extractedDictionary, goldDictionary)
    val majorPrecision: Double = CharacterDictionaryEvaluator.precision(extractedMajorDictionary, goldMajorDictionary)
    val majorRecall: Double = CharacterDictionaryEvaluator.recall(extractedMajorDictionary, goldMajorDictionary)

    Map("#" -> extractedDictionary.size,
      "# Diff." -> CharacterDictionaryEvaluator.numCharacterFraction(extractedDictionary, goldDictionary),
      "P" -> precision,
      "R" -> recall,
      "F" -> Metrics.f1(precision, recall),
      "# (Major)" -> extractedMajorDictionary.size,
      "# Diff. (Major)" -> CharacterDictionaryEvaluator.numCharacterFraction(extractedMajorDictionary,
        goldMajorDictionary),
      "P (Major)" -> majorPrecision,
      "R (Major)" -> majorRecall,
      "F (Major)" -> Metrics.f1(majorPrecision, majorRecall))
  }

  // Evaluates the given dictionary extractor against a gold dictionary, based on a reduced set of metrics, returning a
  // map of the results.
  private def simpleEvaluateDictExtractor(dictExtractor: DictExtractor,
                                          goldDictionary: CharacterDictionary)
                                         (implicit t: Int): Map[String, Double] = {
    val extractedDictionary: CharacterDictionary = dictExtractor.extract(goldDictionary.storyId, withCounts = true)

    val goldMajorDictionary: CharacterDictionary = goldDictionary.asMajor(t)
    val extractedMajorDictionary: CharacterDictionary = extractedDictionary.asMajor(t)

    Map("#" -> extractedDictionary.size,
      "# Diff." -> CharacterDictionaryEvaluator.numCharacterFraction(extractedDictionary, goldDictionary),
      "R" -> CharacterDictionaryEvaluator.recall(extractedDictionary, goldDictionary),
      "# (Major)" -> extractedMajorDictionary.size,
      "# Diff. (Major)" -> CharacterDictionaryEvaluator.numCharacterFraction(extractedMajorDictionary,
        goldMajorDictionary),
      "R (Major)" -> CharacterDictionaryEvaluator.recall(extractedMajorDictionary, goldMajorDictionary))
  }

  // Evaluates the given dictionary extractor against a collection of gold dictionaries, returning a map of the results.
  private def evaluateDictExtractor(dictExtractor: DictExtractor,
                                    goldDictionaries: Map[StoryId, CharacterDictionary])
                                   (implicit t: Int): Map[String, Double] = {
    val extractedDictionaries: Map[StoryId, CharacterDictionary] = goldDictionaries.keys.map(id =>
      id -> dictExtractor.extract(id, withCounts = true)).toMap

    val goldMajorDictionaries: Map[StoryId, CharacterDictionary] = goldDictionaries.mapValues(_.asMajor(t))
    val extractedMajorDictionaries: Map[StoryId, CharacterDictionary] = extractedDictionaries.mapValues(_.asMajor(t))

    val sizes = extractedDictionaries.map({ case (_, ed) => ed.size })

    val fractions = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.numCharacterFraction(extractedDictionaries.get(id).get, gd)
    })

    val precisions = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.precision(extractedDictionaries.get(id).get, gd)
    })

    val recalls = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.recall(extractedDictionaries.get(id).get, gd)
    })

    val f1s = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.f1(extractedDictionaries.get(id).get, gd)
    })

    val majorSizes = extractedMajorDictionaries.map({ case (_, ed) => ed.size })

    val majorFractions = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.numCharacterFraction(extractedMajorDictionaries.get(id).get, gd)
    })

    val majorPrecisions = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.precision(extractedMajorDictionaries.get(id).get, gd)
    })

    val majorRecalls = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.recall(extractedMajorDictionaries.get(id).get, gd)
    })

    val majorF1s = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.f1(extractedMajorDictionaries.get(id).get, gd)
    })

    Map("Avg. #" -> average(sizes),
        "Std. Dev. #" -> stdDev(sizes),
        "Avg. # Diff." -> average(fractions),
        "Std. Dev. # Diff." -> stdDev(fractions),
        "Avg. P" -> average(precisions),
        "Std. Dev. P" -> stdDev(precisions),
        "Avg. R" -> average(recalls),
        "Std. Dev. R" -> stdDev(recalls),
        "Avg. F" -> average(f1s),
        "Std. Dev. F" -> stdDev(f1s),
        "Avg. # (Major)" -> average(majorSizes),
        "Std. Dev. # (Major)" -> stdDev(majorSizes),
        "Avg. # Diff. (Major)" -> average(majorFractions),
        "Std. Dev. # Diff. (Major)" -> stdDev(majorFractions),
        "Avg. P (Major)" -> average(majorPrecisions),
        "Std. Dev. P (Major)" -> stdDev(majorPrecisions),
        "Avg. R (Major)" -> average(majorRecalls),
        "Std. Dev. R (Major)" -> stdDev(majorRecalls),
        "Avg. F (Major)" -> average(majorF1s),
        "Std. Dev. F (Major)" -> stdDev(majorF1s))
  }

  // Evaluates the given dictionary extractor against a collection of gold dictionaries, based on a reduced set of
  // metrics, returning a map of the results.
  private def simpleEvaluateDictExtractor(dictExtractor: DictExtractor,
                                          goldDictionaries: Map[StoryId, CharacterDictionary])
                                         (implicit t: Int): Map[String, Double] = {
    val extractedDictionaries: Map[StoryId, CharacterDictionary] = goldDictionaries.keys.map(id =>
      id -> dictExtractor.extract(id, withCounts = true)).toMap

    val goldMajorDictionaries: Map[StoryId, CharacterDictionary] = goldDictionaries.mapValues(_.asMajor(t))
    val extractedMajorDictionaries: Map[StoryId, CharacterDictionary] = extractedDictionaries.mapValues(_.asMajor(t))

    val sizes = extractedDictionaries.map({ case (_, ed) => ed.size })

    val fractions = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.numCharacterFraction(extractedDictionaries.get(id).get, gd)
    })

    val recalls = goldDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.recall(extractedDictionaries.get(id).get, gd)
    })

    val majorSizes = extractedMajorDictionaries.map({ case (_, ed) => ed.size })

    val majorFractions = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.numCharacterFraction(extractedMajorDictionaries.get(id).get, gd)
    })

    val majorRecalls = goldMajorDictionaries.map({
      case (id, gd) => CharacterDictionaryEvaluator.recall(extractedMajorDictionaries.get(id).get, gd)
    })

    Map("Avg. #" -> average(sizes),
      "Std. Dev. #" -> stdDev(sizes),
      "Avg. # Diff." -> average(fractions),
      "Std. Dev. # Diff." -> stdDev(fractions),
      "Avg. R" -> average(recalls),
      "Std. Dev. R" -> stdDev(recalls),
      "Avg. # (Major)" -> average(majorSizes),
      "Std. Dev. # (Major)" -> stdDev(majorSizes),
      "Avg. # Diff. (Major)" -> average(majorFractions),
      "Std. Dev. # Diff. (Major)" -> stdDev(majorFractions),
      "Avg. R (Major)" -> average(majorRecalls),
      "Std. Dev. R (Major)" -> stdDev(majorRecalls))
  }

}

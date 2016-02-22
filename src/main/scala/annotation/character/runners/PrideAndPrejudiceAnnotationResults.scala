package annotation.character.runners

import java.io.File

import annotation.character.{AccuracyCalculator, CharacterAnnotations}
import annotation.{FrankenbratDocAnnotations, FrankenbratAnnotation}

import org.json4s._
import org.json4s.native.JsonMethods.{parse => parseJSON}
import org.json4s.JsonAST.{JInt, JArray, JString}
import stats.Metrics


/**
 * Calculates and outputs the accuracy results for the Pride and Prejudice character annotations.
 */
object PrideAndPrejudiceAnnotationResults extends App {

  /** List of shared chapter #'s amongst coders. */
//  val SHARED_CHAP_NUMS: List[Int] = List(10, 11, 30, 31, 50, 51, 52)
  // Temporarily consider just chapter 10.
  val SHARED_CHAP_NUMS: List[Int] = List(10)

  val DIRPATH = "data/annotations/character-resolution/pride-and-prejudice"
  
  /**
   * Parses a coder's Frankenbrat annotations, stored in a .json file that lists the document annotations.
   * @param filepath Filepath to .json file with Frankenbrat annotations.
   * @return List of Frankenbrat document annotations.
   */
  def parseFrankenbratAnnotations(filepath: String): List[FrankenbratDocAnnotations] = {
    // Build the list of document annotations document-by-document.
    def build(json: JValue, i: Int, acc: List[FrankenbratDocAnnotations]): List[FrankenbratDocAnnotations] = {
      try {
        val name: String = json(i) \ "name" match {
          case JString(n) => n
          case _ => throw new IllegalArgumentException
        }

        var annotations: Seq[FrankenbratAnnotation] = Seq()
        for {
          JArray(List(JString(id), JString(tag), JArray(List(JArray(List(JInt(startOffset), JInt(endOffset))))))) <-
          json(i) \ "entities"
        } annotations :+= FrankenbratAnnotation(id, tag, startOffset.toInt, endOffset.toInt)

        build(json, i + 1, FrankenbratDocAnnotations(name, annotations)::acc)
      } catch {
        case _: IndexOutOfBoundsException => acc
      }
    }

    val json: JValue = parseJSON(io.Source.fromFile(new File(filepath)).mkString)

    build(json, 0, List[FrankenbratDocAnnotations]())
  }

  /**
   * Filters those character annotations corresponding to shared annotation chapters.
   * @param characterAnnotationsList List of passage character annotations.
   * @return List of character annotations belonging to shared annotation chapters.
   */
  def filterShared(characterAnnotationsList: List[CharacterAnnotations]): List[CharacterAnnotations] =
    characterAnnotationsList.filter(cas => SHARED_CHAP_NUMS.exists(i => cas.id.startsWith("Chapter " + i)))

  /**
   * Transforms a coder's list of Frankenbrat document annotations to a mapping from document id to character
   * annotations, for only shared passages.
   * @param fas List of Frankenbrat document annotations
   * @return Map from document id to character annotations for shared passages.
   */
  def toSharedCharacterAnnotationsMap(fas: List[FrankenbratDocAnnotations]): Map[String, CharacterAnnotations] =
    filterShared(fas.map(new CharacterAnnotations(_))).map(cas => (cas.id, cas)).toMap

  val estherFrankenbratAnnotationsPath = this.DIRPATH + File.separator + "esther" + File.separator + "annotations.json"
  val christyFrankenbratAnnotationsPath = this.DIRPATH + File.separator + "christy" + File.separator +
    "annotations.json"
  val truthFrankenbratAnnotationsPath = this.DIRPATH + File.separator + "truth" + File.separator +
    "annotations.json"
  
  val estherAnnotations: Map[String, CharacterAnnotations] =
    toSharedCharacterAnnotationsMap(parseFrankenbratAnnotations(estherFrankenbratAnnotationsPath))
  val christyAnnotations: Map[String, CharacterAnnotations] =
    toSharedCharacterAnnotationsMap(parseFrankenbratAnnotations(christyFrankenbratAnnotationsPath))
  val truthAnnotations: Map[String, CharacterAnnotations] =
    toSharedCharacterAnnotationsMap(parseFrankenbratAnnotations(truthFrankenbratAnnotationsPath))

  // TODO: Comment.
  val precisions: Map[String, Map[String, Double]] = Map(
    "esther" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.precision(estherAnnotations.get(id).get, gcas)) }),
    "christy" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.precision(christyAnnotations.get(id).get, gcas)) })
  )

  // TODO: Comment.
  val recalls: Map[String, Map[String, Double]] = Map(
    "esther" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.recall(estherAnnotations.get(id).get, gcas)) }),
    "christy" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.recall(christyAnnotations.get(id).get, gcas)) })
  )

  // TODO: Comment.
  val f1s: Map[String, Map[String, Double]] = Map(
    "esther" -> precisions.get("esther").get.map({ case (id, prec) =>
      (id, Metrics.f1(prec, recalls.get("esther").get.get(id).get)) }),
    "christy" -> precisions.get("christy").get.map({ case (id, prec) =>
      (id, Metrics.f1(prec, recalls.get("christy").get.get(id).get)) })
  )

  // TODO: Comment.
  val groupPrecisions: Map[String, Map[String, Double]] = Map(
    "esther" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.groupPrecision(estherAnnotations.get(id).get, gcas)) }),
    "christy" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.groupPrecision(christyAnnotations.get(id).get, gcas)) })
  )

  // TODO: Comment.
  val groupRecalls: Map[String, Map[String, Double]] = Map(
    "esther" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.groupRecall(estherAnnotations.get(id).get, gcas)) }),
    "christy" -> truthAnnotations.map({ case (id, gcas) =>
      (id, AccuracyCalculator.groupRecall(christyAnnotations.get(id).get, gcas)) })
  )

  // TODO: Comment.
  val groupF1s: Map[String, Map[String, Double]] = Map(
    "esther" -> groupPrecisions.get("esther").get.map({ case (id, prec) =>
      (id, Metrics.f1(prec, groupRecalls.get("esther").get.get(id).get)) }),
    "christy" -> groupPrecisions.get("christy").get.map({ case (id, prec) =>
      (id, Metrics.f1(prec, groupRecalls.get("christy").get.get(id).get)) })
  )

  // TODO: Calculate precision, recall, F1-score for the non-character tag.

  // TODO: Properly format results and output to a file.
  println(Metrics.average(precisions.get("esther").get.values))
  println(Metrics.average(precisions.get("christy").get.values))
  println(Metrics.average(recalls.get("esther").get.values))
  println(Metrics.average(recalls.get("christy").get.values))
  println(Metrics.average(f1s.get("esther").get.values))
  println(Metrics.average(f1s.get("christy").get.values))
  println(Metrics.average(groupPrecisions.get("esther").get.values))
  println(Metrics.average(groupPrecisions.get("christy").get.values))
  println(Metrics.average(groupRecalls.get("esther").get.values))
  println(Metrics.average(groupRecalls.get("christy").get.values))
  println(Metrics.average(groupF1s.get("esther").get.values))
  println(Metrics.average(groupF1s.get("christy").get.values))

}

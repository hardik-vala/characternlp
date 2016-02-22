package dictractor.evaluation

import dict.{Alias, Character, CharacterDictionary}
import stats.Metrics

import breeze.optimize.linear.KuhnMunkres


/**
 * Evaluates a character dictionary against another.
 */
object CharacterDictionaryEvaluator {

  implicit val pcom: PrecisionCharacterOverlapMeasure = GoldFraction
  implicit val rcom: RecallCharacterOverlapMeasure = Intersection

  // Returns a matrix with entry (i, j) the overlap score of the ith character in character dictioanry cd1 and the jth
  // character in character dictionary cd2.
  private def overlapMatrix(cd1: CharacterDictionary, cd2: CharacterDictionary)
                           (implicit com: CharacterOverlapMeasure): Vector[Vector[Double]] = {
    val vectors1: Vector[Character] = cd1.characters.toVector
    val vectors2: Vector[Character] = cd2.characters.toVector
    vectors1.map(c1 => vectors2.map(c2 => com.score(c1, c2)))
  }

  // Returns the maximum entry in the given overlap matrix.
  private def maxOverlap(overlapMatrix: Seq[Seq[Double]]): Double = overlapMatrix.flatten.max

  // Returns the minimum of two integers.
  private def pairMin(i1: Int, i2: Int) = if (i1 < i2) i1 else i2

  /**
   * Calculates the precision of a given character dictionary against another.
   * @param extractedDictionary Extracted dictionary to measure precision for.
   * @param goldDictionary Gold dictionary to measure against.
   * @param pcom Character overlap measure for calculating precision.
   * @return Precision.
   */
  def precision(extractedDictionary: CharacterDictionary, goldDictionary: CharacterDictionary)
            (implicit pcom: PrecisionCharacterOverlapMeasure): Double = {
    if (goldDictionary.size == 0)
      return if (extractedDictionary.size == 0) 1.0 else 0.0

    if (extractedDictionary.size == 0)
      return 0.0

    val overlapMatrix: Vector[Vector[Double]] = this.overlapMatrix(extractedDictionary, goldDictionary)(pcom)

    try {
      val maxOverlap: Double = this.maxOverlap(overlapMatrix)

      // "Inverted" overlap matrix, i.e. entries are reversed.
      val invertedOverlapMatrix: Vector[Vector[Double]] = overlapMatrix.map(_.map(maxOverlap - _))

      val numMatchCharacters = this.pairMin(extractedDictionary.size, goldDictionary.size)

      // Use Kuhn-Munkres algorithm to calculate minimum "cost" of any matching between extracted and gold characters
      // according to "inverted" values.
      KuhnMunkres.extractMatching(invertedOverlapMatrix) match {
        // Recover maximum value of any matching by applying the same inversion again and average across the number of
        // extracted characters.
        case (_, minCost) => (numMatchCharacters * maxOverlap - minCost) / extractedDictionary.size
      }
    } catch {
      case e: java.lang.ArrayIndexOutOfBoundsException =>
        // TODO: Handle better.
        e.printStackTrace()
        0.0
    }
  }

  // Returns a matrix with entry (i, j) 1 if the overlap score between the ith character in character dictioanry cd1 and
  // the jth character in character dictionary cd2 is greater than 0, 0 otherwise.
  private def binaryOverlapMatrix(cd1: CharacterDictionary, cd2: CharacterDictionary)
                                 (implicit com: CharacterOverlapMeasure): Vector[Vector[Double]] =
    cd1.characters.toVector.map(c1 => cd2.characters.toVector.map(c2 => if (com.score(c1, c2) > 0) 1.0 else 0.0))

  // Returns 1 if some entry in the binary overlap matrix is 1, otherwise 0.
  private def maxBinaryOverlap(binaryOverlapMatrix: Seq[Seq[Double]]): Double =
    if (binaryOverlapMatrix.flatten.exists(_ > 0.0)) 1.0 else 0.0

  /**
   * Calculates the recall of a given character dictionary against another.
   * @param extractedDictionary Extracted dictionary to measure precision for.
   * @param goldDictionary Gold dictionary to measure against.
   * @param rcom Character overlap measure for calculating recall.
   * @return Recall.
   */
  def recall(extractedDictionary: CharacterDictionary, goldDictionary: CharacterDictionary)
               (implicit rcom: RecallCharacterOverlapMeasure): Double = {
    if (goldDictionary.size == 0)
      return 1.0

    if (extractedDictionary.size == 0)
      return 0.0

    val binaryOverlapMatrix: Vector[Vector[Double]] =
      this.binaryOverlapMatrix(goldDictionary, extractedDictionary)(rcom)
    val maxBinaryOverlap: Double = this.maxBinaryOverlap(binaryOverlapMatrix)

    // "Inverted" overlap matrix, i.e. entries are reversed.
    val invertedOverlapMatrix: Vector[Vector[Double]] = binaryOverlapMatrix.map(_.map(maxBinaryOverlap - _))

    val numMatchCharacters = this.pairMin(extractedDictionary.size, goldDictionary.size)

    // Use Kuhn-Munkres algorithm to calculate minimum "cost" of any matching between extracted and gold characters
    // according to "inverted" values.
    KuhnMunkres.extractMatching(invertedOverlapMatrix) match {
      // Recover maximum value of any matching by applying the same inversion again and average across the number of
      // gold characters.
      case (_, minCost) => (numMatchCharacters * maxBinaryOverlap - minCost) / goldDictionary.size
    }
  }

  /**
   * Calculates the F1-score of the given character dictionary against another.
   * @param extractedDictionary Extracted dictionary to measure the f1-score for.
   * @param goldDictionary Gold dictionary to measure against.
   * @param pcom Character overlap measure for calculating precision.
   * @param rcom Character overlap measure for calculating recall.
   * @return F1-score.
   */
  def f1(extractedDictionary: CharacterDictionary, goldDictionary: CharacterDictionary)
        (implicit pcom: PrecisionCharacterOverlapMeasure, rcom: RecallCharacterOverlapMeasure): Double =
    Metrics.f1(this.precision(extractedDictionary, goldDictionary)(pcom),
      this.recall(extractedDictionary, goldDictionary)(rcom))

  /**
   * Returns the difference, as a fraction of the size of the gold dictionary, between the given extracted and gold
   * dictionaries.
   * @param extractedDictionary Extracted dictionary.
   * @param goldDictionary Gold dictionary.
   * @return Difference as a fraction.
   */
  def numCharacterFraction(extractedDictionary: CharacterDictionary, goldDictionary: CharacterDictionary): Double = {
    val eSize: Int = extractedDictionary.size
    val gSize: Int = goldDictionary.size

    if (eSize == 0 && gSize == 0)
      return 0.0

    (eSize - gSize).toDouble / gSize
  }

  /**
   * Returns the symmetric difference between the aliases of the given extracted and gold character dictionaries.
   * @param extractedDictionary Extracted dictionary.
   * @param goldDictionary Gold dictionary.
   * @return Differing extracted and gold aliases as a map.
   */
  def aliasDiff(extractedDictionary: CharacterDictionary,
                goldDictionary: CharacterDictionary): Map[String, List[Alias]] = {
    val extractedAliases = extractedDictionary.aliases
    val goldAliases = goldDictionary.aliases
    Map("extracted" -> extractedAliases.diff(goldAliases), "gold" -> goldAliases.diff(extractedAliases))
  }

  /**
   * Returns the symmetric difference between the characters of the given extracted and gold character dictionaries.
   * @param extractedDictionary Extracted dictionary.
   * @param goldDictionary Gold dictionary.
   * @return Differing extracted and gold characters as a map.
   */
  def characterDiff(extractedDictionary: CharacterDictionary,
                    goldDictionary: CharacterDictionary): Map[String, Set[Character]] =
    Map("extracted" -> extractedDictionary.characters.filter(ec =>
        goldDictionary.characters.forall(gc => (ec & gc).isEmpty)),
      "gold" -> goldDictionary.characters.filter(gc =>
        extractedDictionary.characters.forall(ec => (gc & ec).isEmpty)))

}

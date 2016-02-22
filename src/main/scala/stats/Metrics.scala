package stats


/**
 * Container for a number of useful metric calculators.
 */
object Metrics {

  /**
   * Calculates the average of the values in an iterable.
   * @param ts Iterable.
   * @param numeric Numeric.
   * @tparam T Type of values in iterable.
   * @return Average of iterable values (as Double).
   */
  def average[T](ts: Iterable[T])(implicit numeric: Numeric[T]) = numeric.toDouble(ts.sum) / ts.size

  /**
   * Calculates the standard deviation of the values in an iterable.
   * @param ts Iterable.
   * @param numeric Numeric.
   * @tparam T Type of values in iterable.
   * @return Standard deviation of iterable values.
   */
  def stdDev[T](ts: Iterable[T])(implicit numeric: Numeric[T]): Double = {
    val avg: Double = this.average(ts)
    val devs: Iterable[Double] = ts.map(t => (numeric.toDouble(t) - avg) * (numeric.toDouble(t) - avg))
    Math.sqrt(devs.sum / ts.size)
  }

  /**
   * Precision.
   * @param numTPs # TP's.
   * @param numFPs # FP's.
   * @return Precision.
   */
  def precision(numTPs: Int, numFPs: Int): Double =
    if (numTPs == 0 && numFPs == 0)
      1.0
    else
      numTPs.toDouble / (numTPs + numFPs)

  /**
   * Recall.
   * @param numTPs # TP's.
   * @param numFNs # FN's.
   * @return Recall.
   */
  def recall(numTPs: Int, numFNs: Int): Double =
    if (numTPs == 0 && numFNs == 0)
      1.0
    else
      numTPs.toDouble / (numTPs + numFNs)

  /**
   * F1-score (from precision and recall values).
   * @param p Precision.
   * @param r Recall.
   * @return F1-score.
   */
  def f1(p: Double, r: Double) = if (p == 0.0 && r == 0.0) 0.0 else 2 * p * r / (p + r)

  /**
   * F1-score (from the number of TP, FP, and FN's).
   * @param numTPs # TP's.
   * @param numFPs # FP's.
   * @param numFNs # FN's.
   * @return F1-score.
   */
  def f1(numTPs: Int, numFPs: Int, numFNs: Int): Double =
    if (numTPs == 0 && numFPs == 0 && numFNs == 0) 0.0 else 2 * numTPs.toDouble / (2 * numTPs + numFPs + numFNs)

}

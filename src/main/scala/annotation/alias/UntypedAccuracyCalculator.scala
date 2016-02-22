package annotation.alias

import stats.Metrics


/**
 * Untyped alias identification accuracy calculator.
 */
object UntypedAccuracyCalculator {

  /**
   * Returns the TP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return List of TP instances.
   */
  def getTPs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): List[UntypedAliasAnnotation] =
    (aliasAnnotations.untypedAnnotations.toSet & trueAliasAnnotations.untypedAnnotations.toSet).toList

  /**
   * Returns the number of TP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return Number of TP instances.
   */
  def getNumTPs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Int =
    this.getTPs(aliasAnnotations, trueAliasAnnotations).size

  /**
   * Returns the FP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return List of FP instances.
   */
  def getFPs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): List[UntypedAliasAnnotation] =
    (aliasAnnotations.untypedAnnotations.toSet &~ trueAliasAnnotations.untypedAnnotations.toSet).toList

  /**
   * Returns the number of FP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return Number of FP instances.
   */
  def getNumFPs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Int =
    this.getFPs(aliasAnnotations, trueAliasAnnotations).size

  /**
   * Returns the FN instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return List of FN instances.
   */
  def getFNs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): List[UntypedAliasAnnotation] =
    (trueAliasAnnotations.untypedAnnotations.toSet &~ aliasAnnotations.untypedAnnotations.toSet).toList

  /**
   * Returns the number of FN instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return Number of FN instances.
   */
  def getNumFNs(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Int =
    this.getFNs(aliasAnnotations, trueAliasAnnotations).size

  /**
   * Returns the precision of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return Precision.
   */
  def precision(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Double =
    Metrics.precision(this.getNumTPs(aliasAnnotations, trueAliasAnnotations),
      this.getNumFPs(aliasAnnotations, trueAliasAnnotations))

  /**
   * Returns the recall of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return Recall.
   */
  def recall(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Double =
    Metrics.recall(this.getNumTPs(aliasAnnotations, trueAliasAnnotations),
      this.getNumFNs(aliasAnnotations, trueAliasAnnotations))

  /**
   * Returns the F1-score of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @return F1-score.
   */
  def f1(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Double =
    Metrics.f1(this.getNumTPs(aliasAnnotations, trueAliasAnnotations),
      this.getNumFPs(aliasAnnotations, trueAliasAnnotations), this.getNumFNs(aliasAnnotations, trueAliasAnnotations))

}

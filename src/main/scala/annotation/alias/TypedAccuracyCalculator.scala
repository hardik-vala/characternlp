package annotation.alias

import stats.Metrics

/**
 * Typed alias identification accuracy calculator.
 */
object TypedAccuracyCalculator {

  /**
   * Returns the TP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return List of TP instances.
   */
  def getTPs(aliasAnnotations: AliasAnnotations,
             trueAliasAnnotations: AliasAnnotations,
             aliasType: Option[String]): List[TypedAliasAnnotation] =
    if (aliasType.isDefined)
      (aliasAnnotations.get(aliasType.get).toSet & trueAliasAnnotations.get(aliasType.get).toSet).toList
    else
      (aliasAnnotations.typedAnnotations.toSet & trueAliasAnnotations.typedAnnotations.toSet).toList

  /**
   * Returns the number of TP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return Number of TP instances.
   */
  def getNumTPs(aliasAnnotations: AliasAnnotations,
                trueAliasAnnotations: AliasAnnotations,
                aliasType: Option[String]): Int =
    this.getTPs(aliasAnnotations, trueAliasAnnotations, aliasType).size

  /**
   * Returns the FP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return List of FP instances.
   */
  def getFPs(aliasAnnotations: AliasAnnotations,
             trueAliasAnnotations: AliasAnnotations,
             aliasType: Option[String]): List[TypedAliasAnnotation] =
    if (aliasType.isDefined)
      (aliasAnnotations.get(aliasType.get).toSet &~ trueAliasAnnotations.get(aliasType.get).toSet).toList
    else
      (aliasAnnotations.typedAnnotations.toSet &~ trueAliasAnnotations.typedAnnotations.toSet).toList

  /**
   * Returns the number of FP instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return Number of FP instances.
   */
  def getNumFPs(aliasAnnotations: AliasAnnotations,
                trueAliasAnnotations: AliasAnnotations,
                aliasType: Option[String]): Int =
    this.getFPs(aliasAnnotations, trueAliasAnnotations, aliasType).size

  /**
   * Returns the FN instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return List of FN instances.
   */
  def getFNs(aliasAnnotations: AliasAnnotations,
             trueAliasAnnotations: AliasAnnotations,
             aliasType: Option[String]): List[TypedAliasAnnotation] =
    if (aliasType.isDefined)
      (trueAliasAnnotations.get(aliasType.get).toSet &~ aliasAnnotations.get(aliasType.get).toSet).toList
    else
      (trueAliasAnnotations.typedAnnotations.toSet &~ aliasAnnotations.typedAnnotations.toSet).toList

  /**
   * Returns the number of FN instances in the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return Number of FN instances.
   */
  def getNumFNs(aliasAnnotations: AliasAnnotations,
                trueAliasAnnotations: AliasAnnotations,
                aliasType: Option[String]): Int =
    this.getFNs(aliasAnnotations, trueAliasAnnotations, aliasType).size

  /**
   * Returns the precision of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return Precision.
   */
  def precision(aliasAnnotations: AliasAnnotations,
                trueAliasAnnotations: AliasAnnotations,
                aliasType: Option[String]): Double =
    Metrics.precision(this.getNumTPs(aliasAnnotations, trueAliasAnnotations, aliasType),
      this.getNumFPs(aliasAnnotations, trueAliasAnnotations, aliasType))

  /**
   * Returns the recall of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return Recall.
   */
  def recall(aliasAnnotations: AliasAnnotations,
             trueAliasAnnotations: AliasAnnotations,
             aliasType: Option[String]): Double =
    Metrics.recall(this.getNumTPs(aliasAnnotations, trueAliasAnnotations, aliasType),
      this.getNumFNs(aliasAnnotations, trueAliasAnnotations, aliasType))

  /**
   * Returns the F1-score of the given alias annotations, as measured against the true annotations.
   * @param aliasAnnotations Alias annotations.
   * @param trueAliasAnnotations True alias annotations.
   * @param aliasType The alias type to consider. If None, then all alias types are considered.
   * @return F1-score.
   */
  def f1(aliasAnnotations: AliasAnnotations,
         trueAliasAnnotations: AliasAnnotations,
         aliasType: Option[String]): Double =
    Metrics.f1(this.getNumTPs(aliasAnnotations, trueAliasAnnotations, aliasType),
      this.getNumFPs(aliasAnnotations, trueAliasAnnotations, aliasType),
      this.getNumFNs(aliasAnnotations, trueAliasAnnotations, aliasType))

  // TODO
  def typeAccuracy(aliasAnnotations: AliasAnnotations, trueAliasAnnotations: AliasAnnotations): Double = {
    val commonUntypedAliasAnnotations: List[TypedAliasAnnotation] = aliasAnnotations.typedAnnotations.filter(taa1 =>
      trueAliasAnnotations.typedAnnotations.toSet.exists(_.untypedEquals(taa1)))

    ((trueAliasAnnotations.typedAnnotations.toSet & commonUntypedAliasAnnotations.toSet).size.toDouble /
      commonUntypedAliasAnnotations.size)
  }

}

package network.alias.detection

import network.alias.{AliasManager, Alias}
import stats.Metrics


/**
 * Evaluator for alias detection.
 */
object AliasDetectionEvaluator {

  /**
   * Returns the TP instances in the aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return List of TP instances.
   */
  def getTPs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): List[Alias] =
    (aliases.toSet & trueAliases.toSet).toList

  /**
   * Returns the number of TP instances in the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return Number of TP instances.
   */
  def getNumTPs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Int = this.getTPs(aliases, trueAliases).size

  /**
   * Returns the FP instances in the aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return List of FP instances.
   */
  def getFPs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): List[Alias] =
    (aliases.toSet &~ trueAliases.toSet).toList

  /**
   * Returns the number of FP instances in the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return Number of FP instances.
   */
  def getNumFPs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Int = this.getFPs(aliases, trueAliases).size

  /**
   * Returns the FN instances in the aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return List of FN instances.
   */
  def getFNs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): List[Alias] =
    (trueAliases.toSet &~ aliases.toSet).toList

  /**
   * Returns the number of FN instances in the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return Number of FN instances.
   */
  def getNumFNs(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Int = this.getFNs(aliases, trueAliases).size

  /**
   * Returns the precision of the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return Precision.
   */
  def precision(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Double =
    Metrics.precision(this.getNumTPs(aliases, trueAliases), this.getNumFPs(aliases, trueAliases))

  /**
   * Returns the recall of the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return Recall.
   */
  def recall(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Double =
    Metrics.recall(this.getNumTPs(aliases, trueAliases), this.getNumFNs(aliases, trueAliases))

  /**
   * Returns the F1-score of the given aliases, as measured against the true aliases.
   * @param aliases Aliases.
   * @param trueAliases True aliases.
   * @return F1-score.
   */
  def f1(aliases: Iterable[Alias], trueAliases: Iterable[Alias]): Double =
    Metrics.f1(this.getNumTPs(aliases, trueAliases), this.getNumFPs(aliases, trueAliases),
      this.getNumFNs(aliases, trueAliases))

}

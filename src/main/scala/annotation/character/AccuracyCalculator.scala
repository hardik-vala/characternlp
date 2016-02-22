package annotation.character

import stats.Metrics


/**
 * Character annotations' accuracy calculator.
 */
object AccuracyCalculator {

  /**
   * Returns the TP instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return List of TP instances.
   */
  def getTPs(characterAnnotations: CharacterAnnotations,
             trueCharacterAnnotations: CharacterAnnotations,
             tag: Option[String] = None): List[CharacterAnnotation] =
    if (tag.isDefined)
      (characterAnnotations.get(tag.get).toSet & trueCharacterAnnotations.get(tag.get).toSet).toList
    else
      characterAnnotations & trueCharacterAnnotations

  /**
   * Returns the number of TP instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return Number of TP instances.
   */
  def getNumTPs(characterAnnotations: CharacterAnnotations,
                trueCharacterAnnotations: CharacterAnnotations,
                tag: Option[String] = None): Int =
    this.getTPs(characterAnnotations, trueCharacterAnnotations, tag).size

  /**
   * Returns the FP instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return List of FP instances.
   */
  def getFPs(characterAnnotations: CharacterAnnotations,
             trueCharacterAnnotations: CharacterAnnotations,
             tag: Option[String] = None): List[CharacterAnnotation] =
    if (tag.isDefined)
      (characterAnnotations.get(tag.get).toSet &~ trueCharacterAnnotations.get(tag.get).toSet).toList
    else
      characterAnnotations &~ trueCharacterAnnotations

  /**
   * Returns the number of FP instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return Number of FP instances.
   */
  def getNumFPs(characterAnnotations: CharacterAnnotations,
                trueCharacterAnnotations: CharacterAnnotations,
                tag: Option[String] = None): Int =
    this.getFPs(characterAnnotations, trueCharacterAnnotations, tag).size

  /**
   * Returns the FN instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return List of FN instances.
   */
  def getFNs(characterAnnotations: CharacterAnnotations,
             trueCharacterAnnotations: CharacterAnnotations,
             tag: Option[String] = None): List[CharacterAnnotation] =
    if (tag.isDefined)
      (trueCharacterAnnotations.get(tag.get).toSet &~ characterAnnotations.get(tag.get).toSet).toList
    else
      trueCharacterAnnotations &~ characterAnnotations

  /**
   * Returns the number of FN instances in the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return Number of FN instances.
   */
  def getNumFNs(characterAnnotations: CharacterAnnotations,
                trueCharacterAnnotations: CharacterAnnotations,
                tag: Option[String] = None): Int =
    this.getFNs(characterAnnotations, trueCharacterAnnotations, tag).size

  /**
   * Returns the precision of the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return Precision.
   */
  def precision(characterAnnotations: CharacterAnnotations,
                trueCharacterAnnotations: CharacterAnnotations,
                tag: Option[String] = None): Double =
    Metrics.precision(this.getNumTPs(characterAnnotations, trueCharacterAnnotations, tag),
      this.getNumFPs(characterAnnotations, trueCharacterAnnotations, tag))

  /**
   * Returns the recall of the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return Recall.
   */
  def recall(characterAnnotations: CharacterAnnotations,
             trueCharacterAnnotations: CharacterAnnotations,
             tag: Option[String] = None): Double =
    Metrics.recall(this.getNumTPs(characterAnnotations, trueCharacterAnnotations, tag),
      this.getNumFNs(characterAnnotations, trueCharacterAnnotations, tag))

  /**
   * Returns the F1-score of the given character annotations, as measured against the true annotations.
   * @param characterAnnotations Character annotations.
   * @param trueCharacterAnnotations True character annotations.
   * @param tag The tag to consider. If None, then all tags are considered.
   * @return F1-score.
   */
  def f1(characterAnnotations: CharacterAnnotations,
         trueCharacterAnnotations: CharacterAnnotations,
         tag: Option[String] = None): Double =
    Metrics.f1(this.getNumTPs(characterAnnotations, trueCharacterAnnotations, tag),
      this.getNumFPs(characterAnnotations, trueCharacterAnnotations, tag),
      this.getNumFNs(characterAnnotations, trueCharacterAnnotations, tag))

  // TODO: Comment.
  private def getGroupCharacterAnnotations(characterAnnotations: CharacterAnnotations,
                                           trueCharacterAnnotations: CharacterAnnotations):
                                              (CharacterAnnotations, CharacterAnnotations) = {
    // Set of offset pairs for character annotations corresponding to group aliases, shared across the two sequences of
    // annotations.
    val groupOffsets: Set[(Int, Int)] = characterAnnotations.group.map(_.offsets).toSet |
      trueCharacterAnnotations.group.map(_.offsets).toSet

    val groupCharacterAnnotations = CharacterAnnotations(characterAnnotations.id,
      characterAnnotations.annotations.filter(ca => groupOffsets.contains(ca.offsets)))
    val trueGroupCharacterAnnotations = CharacterAnnotations(trueCharacterAnnotations.id,
      trueCharacterAnnotations.annotations.filter(ca => groupOffsets.contains(ca.offsets)))

    (groupCharacterAnnotations, trueGroupCharacterAnnotations)
  }

  // TODO: Comment.
  def groupPrecision(characterAnnotations: CharacterAnnotations,
                     trueCharacterAnnotations: CharacterAnnotations): Double =
    getGroupCharacterAnnotations(characterAnnotations, trueCharacterAnnotations) match {
      case (groupCharacterAnnotations, trueGroupCharacterAnnotations) => this.precision(groupCharacterAnnotations,
        trueGroupCharacterAnnotations)
    }

  // TODO: Comment.
  def groupRecall(characterAnnotations: CharacterAnnotations,
                  trueCharacterAnnotations: CharacterAnnotations): Double =
    getGroupCharacterAnnotations(characterAnnotations, trueCharacterAnnotations) match {
      case (groupCharacterAnnotations, trueGroupCharacterAnnotations) => this.recall(groupCharacterAnnotations,
        trueGroupCharacterAnnotations)
    }

}

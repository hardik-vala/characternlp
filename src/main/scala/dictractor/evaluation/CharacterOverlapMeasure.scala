package dictractor.evaluation

import dict.Character

/**
 * For measuring the overlap score between the alias set of characters (i.e. extracted and gold) for the purposes of
 * evaluation.
 */
trait CharacterOverlapMeasure {

  /**
   * Calculates the overlap score between two characters.
   * @param c1 First character.
   * @param c2 Second character.
   * @return Score as a Double.
   */
  def score(c1: Character, c2: Character): Double

}

trait PrecisionCharacterOverlapMeasure extends CharacterOverlapMeasure
trait RecallCharacterOverlapMeasure extends CharacterOverlapMeasure

/**
 * Jaccard index character overlap measure .
 */
class JaccardIndex extends CharacterOverlapMeasure {

  /**
   * Scores two characters according to the Jaccard index of their respective alias sets.
   * @param c1 First character.
   * @param c2 Second character.
   * @return Jaccard index.
   */
  override def score(c1: Character, c2: Character): Double = (c1 & c2).size.toDouble / (c1 | c2).size

}

/**
 * Calculates the fraction of gold aliases.
 */
object GoldFraction extends PrecisionCharacterOverlapMeasure {

  /**
   * Calculates the fraction of gold aliases in the aliases for the given character.
   * @param character Character.
   * @param goldCharacter Gold character.
   * @return Fraction as a Double.
   */
  override def score(character: Character, goldCharacter: Character): Double =
    (character & goldCharacter).size.toDouble / character.size
}

/**
 * The coefficient of gold characters-based overlap measure.
 */
object GoldCoefficient extends PrecisionCharacterOverlapMeasure {

  /**
   * Returns the fraction of the number of aliases in the gold character over the union of all aliases between the
   * gold and given character.
   * @param character Character.
   * @param goldCharacter Gold character.
   * @return Fraction as a Double.
   */
  override def score(character: Character, goldCharacter: Character): Double =
    goldCharacter.size.toDouble / (character | goldCharacter).size
}

/**
 * Intersection character overlap measure.
 */
object Intersection extends RecallCharacterOverlapMeasure {

  /**
   * Calculates the amount of overlap between the aliases of two characters.
   * @param c1 First character.
   * @param c2 Second character.
   * @return Amount of overlap as a Double.
   */
  override def score(c1: Character, c2: Character): Double = (c1 & c2).size.toDouble

}

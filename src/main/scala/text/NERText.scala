package text

import scala.util.matching.Regex


/**
 * Stanford NER labels.
 */
sealed trait NERLabel { override def toString: String }

/** Stanford NER Location label. */
case object LOCATION extends NERLabel {

  override def toString: String = "LOCATION"

}

/** Stanford NER Organization label. */
case object ORGANIZATION extends NERLabel {

  override def toString: String = "ORGANIZATION"

}

/** Stanford NER Person label. */
case object PERSON extends NERLabel {

  override def toString: String = "PERSON"

}


/**
 * Offers a number of useful functions for operating on NER annotated texts.
 */
object NERText {

  // Returns the regex for the given token according to the given NER label.
  private def labelRegex(t: String, l: NERLabel): Regex = ("\\W" + t + "/" + l + "\\W").r

  // Counts the number of occurrences for the given token with the given NER label in the given NER annotated text.
  private def countToken(t: String, l: NERLabel, nerAnnotatedText: String): Int =
    if (t.contains("*")) 0 else labelRegex(t, l).findAllIn(nerAnnotatedText).size

  // Counts the number of occurrences for the given string span with the given NER label in the given NER annotated
  // text, calculated as the number of occurrences of each token in the span with the given label in the given text.
  private def countSpan(s: String, l: NERLabel, nerAnnotatedText: String): Int =
    s.split("\\s+").map(t => countToken(t, l, nerAnnotatedText)).sum

  /**
   * Returns the label most frequently assigned to the given string in the given NER annotated text.
   * @param s String.
   * @param nerAnnotatedText NER annotated text.
   * @return Majority NER label (Ties are resolved randomly).
   */
  def getMajorityLabel(s: String, nerAnnotatedText: String): NERLabel =
    List(LOCATION, ORGANIZATION, PERSON).map(l => (l, this.countSpan(s, l, nerAnnotatedText))).maxBy(_._2)._1

  /**
   * Checks if the given string is assigned PERSON in the given NER annotated text.
   * @param s String.
   * @param nerAnnotatedText NER anntoated text.
   * @return True if the string is given a majority label of PERSON, false otherwise.
   */
  def isPerson(s: String, nerAnnotatedText: String): Boolean = this.getMajorityLabel(s, nerAnnotatedText) == PERSON

}

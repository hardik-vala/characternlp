package name

import lang.Pronouns
import utils.Utils._


/**
 * Offers useful functions operating on names.
 */
object Name {

  /** Set of name "of"s. */
  lazy val ofs = Set("de", "da", "van", "von")

  // Checks if the given string starts with a capital letter.
  private def isCapitalized(s: String) = s(0).isUpper

  // Checks if the given string is a single letter abbreviation.
  private def isAbbreviation(s: String) = s.matches("[A-Z]\\.")

  // Checks if the given string is a name by simply checking that the first and last tokens are capitalized and that
  // the string is not all uppercase, or the given string is a single letter abbreviation.
  def isName(s: String): Boolean = {
    val tokens = s.split("\\s+")
    val pronouns = Pronouns.get
    ((s.toUpperCase != s) && !pronouns.contains(s) && (this.isCapitalized(tokens(0)) || this.ofs.contains(tokens(0))) &&
      this.isCapitalized(tokens(tokens.length - 1))) ||
      this.isAbbreviation(s)
  }

  case class NameToken(t: String) {
    override def equals(o: Any): Boolean = o match {
      case nt: NameToken =>
        if (Name.isAbbreviation(t))
          t == nt.t(0) + "."
        else if (Name.isAbbreviation(nt.t))
          nt.t == t(0) + "."
        else
          t == nt.t
      case _ => false
    }
  }

  /**
   * Checks whether the given pair of names are name variants of each other or not.
   * @param n1 Name #1.
   * @param n2 Name #2.
   * @return True if the names are variants of each other, false otherwise.
   */
  def areVariants(n1: String)(n2: String): Boolean = {
    if (this.isName(n1) && this.isName(n2)) {
      val tokens1: List[NameToken] = NameTitles
        .stripTitles(n1)
        // Ignore single letter abbreviations.
        //      .replaceAll("[^\\s][A-Z]\\.\\s", " ")
        .trim
        .split("\\s+")
        .map(NameToken)
        .toList

      val tokens2: List[NameToken] = NameTitles
        .stripTitles(n2)
        // Ignore single letter abbreviations.
        //      .replaceAll("[^\\s][A-Z]\\.\\s", " ")
        .trim
        .split("\\s+")
        .map(NameToken)
        .toList

      isSubSeq(tokens1, tokens2) || isSubSeq(tokens2, tokens1)
    } else
      false
  }

  /**
   * Returns the first name of the given name.
   * @param n Name.
   * @return First name.
   */
  def getFirstName(n: String): String = NameTitles.stripTitles(n).split("\\s+")(0)

}

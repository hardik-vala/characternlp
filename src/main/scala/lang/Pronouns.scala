package lang


/**
 * Manages set of determiners.
 */
object Pronouns {

  // Base pronoun representations.
  private val basePronouns: Set[String] =
    Set("I", "Me", "He", "She", "Him", "You", "Himself", "Herself", "Myself", "Yourself", "Whom", "They", "Them", "Us",
      "We", "Either", "Themselves", "Ourselves", "Yourselves", "Each other", "One another", "One", "Ones", "Each",
      "Her", "It", "Who", "Any", "Some", "Those", "Other", "Others", "Anyone", "Any one", "Someone", "Some one",
      "Anybody", "Everybody", "Every body", "Somebody", "Whichever", "Former", "Latter", "All", "Rest")

  /**
   * Retrieves the set of base pronouns (Capitalized).
   * @return Set of base pronouns (Capitalized).
   */
  def get: Set[String] = this.basePronouns

  /**
   * Retrieves the set of confident pronouns (Capitalized), i.e. those likely to refer to animate ojects.
   * @return Set of confident pronouns (Capitalized).
   */
  def confident: Set[String] =
    Set("I", "Me", "He", "She", "Him", "You", "Himself", "Herself", "Myself", "Yourself", "Whom", "They", "Us",
    "We", "Themselves", "Ourselves", "Yourselves", "Her", "Who")

  /**
   * Retrieves the set of pronouns in all cases.
   * @return Set of pronouns in all cases.
   */
  def getAll: Set[String] = this.basePronouns |
    this.basePronouns.map(_.toLowerCase()) |
    this.basePronouns.map(_.toUpperCase)

  /**
   * Retrieves the set of confident pronouns (all cases), i.e. those likely to refer to animate ojects.
   * @return Set of confident pronouns (all cases).
   */
  def allConfident: Set[String] =
    this.confident |
      this.confident.map(_.toLowerCase()) |
      this.confident.map(_.toUpperCase)

  /**
   * Checks whether the given string is a pronoun.
   * @param s String to check.
   * @return True if the string is a pronoun, false otherwise.
   */
  def isPronoun(s: String): Boolean = this.getAll.contains(s)

  /**
   * Checks whether the given string starts with a pronoun (Assuming regular text spacing).
   * @param s String to check.
   * @return True if the string starts with a pronoun, false otherwise.
   */
  def startsWithPronoun(s: String): Boolean = this.getAll.exists(r => s.startsWith(r + " ") ||
    s.startsWith(r.toLowerCase + " ") || s.startsWith(r.toUpperCase + " "))

  /**
   * Drops the starting pronoun if there is any.
   * @param s String.
   * @return String without starting pronoun.
   */
  def dropStartingPronoun(s: String): String = {
    if (this.startsWithPronoun(s)) {
      val tokens: Array[String] = s.split("\\s+")
      if (tokens.length > 1) {
        tokens.slice(1, tokens.length).mkString(" ")
      } else
        ""
    } else
      s
  }

}

package lang


/**
 * Manages set of determiners.
 */
object Determiners {

  // Base determiner representations.
  private val baseDeterminers: Set[String] = Set("A", "An", "The", "This")

  /**
   * Retrieves the set of determiners in all cases.
   * @return Set of determiners in all cases.
   */
  def get: Set[String] = this.baseDeterminers |
    this.baseDeterminers.map(_.toLowerCase()) |
    this.baseDeterminers.map(_.toUpperCase)

  /**
   * Checks whether the given string is a determiner.
   * @param s String to check.
   * @return True if the string is a determiner, false otherwise.
   */
  def isDeterminer(s: String): Boolean = this.get.contains(s)

  /**
   * Checks whether the given string starts with a determiner (Assuming regular text spacing).
   * @param s String to check.
   * @return True if the string starts with a determiner, false otherwise.
   */
  def startsWithDeterminer(s: String): Boolean = this.get.exists(d => s.startsWith(d + " ") ||
    s.startsWith(d.toLowerCase + " ") || s.startsWith(d.toUpperCase + " "))

  /**
   * Drops the starting determiner if there is any.
   * @param s String.
   * @return String without starting determiner.
   */
  def dropStartingDeterminer(s: String): String = {
    if (this.startsWithDeterminer(s)) {
      val tokens: Array[String] = s.split("\\s+")
      if (tokens.length > 1) {
        tokens.slice(1, tokens.length).mkString(" ")
      } else
        ""
    } else
      s
  }

}

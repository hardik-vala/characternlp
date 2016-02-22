package lang

import java.io.File

import data.Data


/**
 * Maintains list of adjectives.
 */
object Adjectives {

  /** Path to directory with adjectives. */
  val DIRPATH = Data.DIRPATH + File.separator + "adjectives"

  /** Filename for file with adjectives. */
  val FILENAME = "adjectives.txt"

  /** Set of adjectives (Lowercase). */
  lazy val adjectives: Set[String] = this.load(this.DIRPATH + File.separator + this.FILENAME)

  // Loads the adjectives from a file located by the given filepath, where each adjective is listed on its own line.
  private def load(filepath: String): Set[String] =
    io.Source.fromFile(new File(filepath)).getLines().map(_.trim).filterNot(_.isEmpty).toSet

  /**
   * Checks whether the given token string is an adjective.
   * @param t Token string.
   * @return True if the token string is an adjective, false otherwise.
   */
  def isAdjective(t: String): Boolean = this.adjectives.contains(t.toLowerCase)

}

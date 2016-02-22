package name

import java.io.File


object Hypocorisms {

  /** Filename for hypocorisms file. */
  val FILENAME = "hypocorisms.txt"

  // Loads the hypocorisms from a file located by the given filepath, with the hypocorisms for a given name on each line
  // separated by a space.
  private def loadHypocorisms(filepath: String): Set[Set[String]] =
    io.Source.fromFile(new File(filepath))
      .getLines()
      .map(_.split("\\s+").map(_.trim).filter(_.nonEmpty).toSet)
      .filter(_.nonEmpty)
      .toSet

  /**
   * Set of Hypocorisms for each name, represented as a set of strings.
   */
  lazy val hypocorisms = this.loadHypocorisms(NameGazetteer.DIRPATH + File.separator + this.FILENAME)

  /**
   * Checks if the two names are hypocorisms of each other.
   * @param name1 Name #1.
   * @param name2 Name #2.
   * @return True if the names are hypocorisms, false otherwise.
   */
  def areHypocorisms(name1: String)(name2: String): Boolean = {
    val firstName1 = Name.getFirstName(name1)
    val firstName2 = Name.getFirstName(name2)

    this.hypocorisms.exists(h => h.contains(firstName1) && h.contains(firstName2))
  }

}

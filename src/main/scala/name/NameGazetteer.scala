package name

import java.io.File

import data.Data


/**
 * Name gazetteer manager.
 */
object NameGazetteer {

  /** Path to directory with names. */
  val DIRPATH = Data.DIRPATH + File.separator + "name-gazetteer"

  /** Filename for file with female names. */
  val FEMALE_NAMES_FILENAME = "female.txt"

  /** Filename for file with male names. */
  val MALE_NAMES_FILENAME = "male.txt"

  // Loads the names from a file located by the given filepath, with each name listed on its own line.
  private def loadNames(filepath: String): Set[String] =
    io.Source.fromFile(new File(filepath))
      .getLines()
      .map(_.trim)
      // Filter out comments and empty lines.
      .filterNot(l => l.startsWith("# ") || l.isEmpty)
      .toSet

  /**
   * Returns the set of female names.
   * @return Set of female names.
   */
  def femaleNames: Set[String] = this.loadNames(this.DIRPATH + File.separator + this.FEMALE_NAMES_FILENAME)

  /**
   * Returns the set of male names.
   * @return Set of male names.
   */
  def maleNames: Set[String] = this.loadNames(this.DIRPATH + File.separator + this.MALE_NAMES_FILENAME)

  /**
   * Returns the set of unisex names.
   * @return
   */
  def unisexNames: Set[String] = this.femaleNames & this.maleNames

  /** Set of all names. */
  lazy val names: Set[String] = this.maleNames | this.femaleNames

  /** Set of strictly female names (i.e. those that are not unisex). */
  lazy val strictFemaleNames: Set[String] = this.femaleNames &~ this.unisexNames

  /** Set of strictly male names (i.e. those that are not unisex). */
  lazy val strictMaleNames: Set[String] = this.maleNames &~ this.unisexNames

  // Checks whether the given string contains a name from the set of names.
  private def hasName(s: String, names: Set[String]): Boolean = s.split("\\s+").exists(names.contains)

  /**
   * Checks if the given string contains a name.
   * @param s String to check.
   * @return True if the given string contains a name, false otherwise.
   */
  def hasName(s: String): Boolean = this.hasName(s, this.names)

  /**
   * Checks whether the given string contains a strictly female name.
   * @param s String to check.
   * @return True if the given string contains a strictly female name, false otherwise.
   */
  def hasStrictFemaleName(s: String): Boolean = this.hasName(s, this.strictFemaleNames)

  /**
   * Checks whether the given string contains a strictly male name.
   * @param s String to check.
   * @return True if the given string cotnains a strictly male name, false otherwise.
   */
  def hasStrictMaleName(s: String): Boolean = this.hasName(s, this.strictMaleNames)

}

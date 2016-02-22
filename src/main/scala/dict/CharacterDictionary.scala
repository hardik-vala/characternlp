package dict

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import story.StoryId
import text.{ORIG, TextManager}
import utils.Utils


/**
 * Container for the character dictionary for a given story.
 * @param storyId Id of the story.
 * @param characters Set of characters.
 */
class CharacterDictionary(val storyId: StoryId, val characters: Set[Character]) {
  import CharacterDictionary._

  // Stores the count of each alias in the given story, so the mapping doesn't have to be repeatedly generated.
  // Initialized to None.
  private var aliasCounts: Option[Map[Alias, Int]] = None

  /**
   * Returns a list of all the aliases of all characters.
   * @return List of all aliases of all characters (In no particular order).
   */
  def aliases: List[Alias] = this.characters.flatMap(_.aliases).toList

  /**
   * Returns a list of the all the unique alias spans, sorted in terms of decreasing length.
   * @return List of unique alias spans, sorted according to decreasing length.
   */
  def aliasSpans: List[String] = this.aliases.map(_.span).distinct.sortWith(Utils.stringLengthComparator)

  /**
   * Checks if the given span corresponds to any alias in the dictionary.
   * @param span Span to check for.
   * @return True if it is contained in the dictionary, false otherwise.
   */
  def hasAliasSpan(span: String): Boolean = this.aliasSpans.contains(span)

  /**
   * Checks whether the alias counts have been initialized or not.
   * @return True if alias counts have been initialized, false otherwise.
   */
  def hasAliasCounts: Boolean = this.aliasCounts.isDefined

  /**
   * Retrieves a mapping between each alias and its number of occurrences in the given story text.
   * @return A map from alias to the number of occurrences in the given story text.
   */
  def getAliasCounts: Map[Alias, Int] = {
    if (aliasCounts.isEmpty) {
      val aliasToken: String = "<ALIAS>"

      var s = TextManager.getText(storyId, ORIG).replace('\n',' ')
      var aliasSpanCounts: Map[String, Int] = Map()

      this.aliasSpans.foreach((as: String) => {
        aliasSpanCounts += (as -> ("\\W" + as + "\\W").r.findAllMatchIn(s).length)
        s = ("\\W" + as + "\\W").r.replaceAllIn(s, aliasToken)
      })

      this.aliasCounts = Some(this.aliases.map((a: Alias) => a -> aliasSpanCounts.get(a.span).get).toMap)
    }

    this.aliasCounts.get
  }

  /**
   * Sets the alias counts.
   * @param aliasCounts Alias counts.
   */
  def setAliasCounts(aliasCounts: Map[Alias, Int]): Unit = this.aliasCounts = Some(aliasCounts)

  /**
   * Gets the number of occurrences in the given story for the given alias.
   * @param alias Alias.
   * @return Number of occurrences.
   */
  def getAliasCount(alias: Alias): Option[Int] = this.getAliasCounts.get(alias)

  /**
   * Returns the number of characters in the dictionary.
   * @return Number of characters.
   */
  def size: Int = this.characters.size

  /**
   * Checks whether the two spans correspond to two aliases for a given character.
   * @param span1 First span.
   * @param span2 Seconds span.
   * @return True if the two spans are contained in the character, false otherwise.
   */
  def isSameCharacter(span1: String, span2: String): Boolean = this.characters.exists(c => {
    val aliasSpans = c.getAliasSpans().toSet
    aliasSpans.contains(span1) && aliasSpans.contains(span2)
  })

  /**
   * Returns a new character dictionary with only the major characters, i.e. those that occur more than t times.
   * @param t Threshold for major characters.
   * @return Character dictionary with major characters.
   */
  def asMajor(t: Int): CharacterDictionary = {
    val majorCharacters: Set[Character] = this.characters.filter(_.aliases.map(this.getAliasCount(_).get).sum > t)
    new CharacterDictionary(this.storyId, majorCharacters)
  }

  /**
   * Returns a new character dictionary with only the minor characters, i.e. those that occur at most t times.
   * @param t Threshold for minor characters.
   * @return Character dictionary with minor characters.
   */
  def asMinor(t: Int): CharacterDictionary = {
    val minorCharacters: Set[Character] = this.characters.filter(_.aliases.map(this.getAliasCount(_).get).sum <= t)
    new CharacterDictionary(this.storyId, minorCharacters)
  }

  /**
   * Writes the character dictionary in CSV format to the file located at the given file path. The dictionary can be
   * optionally stored with the counts of each alias, which will appear in the .csv in a column entry immediately
   * following the given alias.
   * @param filepath Filepath of output .csv file.
   */
  def toCSV(filepath: String, withCounts: Boolean = false): Unit = {
    val writer = CSVWriter.open(new File(filepath))

    if (withCounts) {
      writer.writeAll(characters.toList.sortWith(comparator).map(c => {
        var row: List[String] = List()
        c.aliases.toList.sortWith(Alias.aliasSpanComparator).foreach(a =>
          row ++= List(a.span, this.getAliasCount(a).get.toString))
        row
      }))
    } else
      writer.writeAll(characters.toList.sortWith(comparator).map(_.getAliasSpans(sorted = true)))

    writer.close()
  }

  override def toString: String = characters.toList.sortWith(comparator).mkString("\n")

}

object CharacterDictionary {

  // Character comparator.
  private def comparator: (Character, Character) => Boolean = (character1: Character, character2: Character) =>
    character1.toString < character2.toString

  /**
   * Checks whether the saved character dictionary located by the given filepath, and stored in standard .csv format,
   * has the alias counts stored as well.
   * @param filepath Filepath to character dictionary .csv file.
   * @return True if the saved dictionary file contains the alias counts as well, false otherwise.
   */
  def CSVHasAliasCounts(filepath: String): Boolean = {
    // # of aliases stored in the dictionary.
    val numAliases: Int = CSVReader.open(new File(filepath)).all().map(_.count(!_.matches("\\d+"))).sum
    // # of counts stored in the dictionary.
    val numCounts: Int = CSVReader.open(new File(filepath)).all().map(_.count(_.matches("\\d+"))).sum
    // If the number of aliases and counts are the same, then we say that the dictionary has alias counts stored.
    numAliases == numCounts
  }

  /**
   * Retrieves the mapping of aliases to counts from the character dictionary stored, with alias counts, at the given
   * filepath in standard .csv format.
   * @param filepath Filepath of character dictionary.
   * @return Map of aliases to corresponding counts.
   */
  def aliasCountsFromCSV(filepath: String): Map[Alias, Int] = {
    val reader = CSVReader.open(new File(filepath))
    var aliasCounts: Map[Alias, Int] = Map()

    reader.foreach((row: Seq[String]) => {
      val rowArray: Array[String] = row.toArray
      var i = 0;
      for (i <- 0 to rowArray.length - 2 by 2)
        aliasCounts += (new Alias(rowArray(i)) -> rowArray(i + 1).toInt)
    })

    reader.close()
    aliasCounts
  }

  /**
   * Retrieves the set of characters in the character dictionary stored in the standard .csv format and located by the
   * given filepath.
   * @param filepath Filepath of character dictionary .csv.
   * @return A pair with the set of extracted characters and the alias counts, if that information is stored as well,
   *         otherwise just None for the alias counts.
   */
  def charactersFromCSV(filepath: String): (Set[Character], Option[Map[Alias, Int]]) = {
    val withCounts = this.CSVHasAliasCounts(filepath)

    var characters: Set[Character] = null
    val reader = CSVReader.open(new File(filepath))

    if (withCounts)
      characters = reader.all().map(c =>
        new Character(c.filterNot(_.matches("\\d+")).map(a => new Alias(a)).toSet)).toSet
    else
      characters = reader.all().map(c => new Character(c.map(a => new Alias(a)).toSet)).toSet

    reader.close()

    if (withCounts) (characters, Some(this.aliasCountsFromCSV(filepath))) else (characters, None)
  }

}

package dict

import io.Source._

import java.io.File

import com.github.tototoshi.csv.CSVReader
import story.{StoryNotFoundException, StoryId, StoryOverseer}
import story.StorySet._


/**
 * Manager for character dictionaries across all stories.
 */
object CharacterDictionaryManager {

  // Name of character dictionary directory across most story sets.
  private val DICTS_DIRNAME = "char-dicts"

  /** Path to MS character dictionary directory. */
  val MS_DIRPATH = StoryOverseer.getPath(MOONSTONE) + File.separator + DICTS_DIRNAME
  /** Path to raw MS dictionary. */
  val MS_RAW_PATH = MS_DIRPATH + File.separator + "raw-corrected.csv"
  /** Path to annotated MS dictionary. */
  val MS_ANOT_PATH = MS_DIRPATH + File.separator + "annotated.csv"
  // Path to the original MS dictionary (in standard format) with alias frequencies.
//  private val MS_ORIG_FREQS_PATH = MS_DIRPATH + File.separator + "orig-wt-freqs.csv"

  /** Path to PP character dictionary directory. */
  val PP_DIRPATH = StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + DICTS_DIRNAME
  /** Path to raw PP dictionary. */
  val PP_RAW_PATH = PP_DIRPATH + File.separator + "raw-corrected.csv"
  /** Path to annotated PP dictionary. */
  val PP_ANOT_PATH = PP_DIRPATH + File.separator + "annotated.csv"
  // Path to the original PP dictionary (in standard format) with alias frequencies.
//  private val PP_ORIG_FREQS_PATH = PP_DIRPATH + File.separator + "orig-wt-freqs.csv"

  /** Path to SH character dictionary directory. */
  val SH_DIRPATH = StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + DICTS_DIRNAME
  /** Path to SH directory with raw dictionaries. */
  val SH_RAW_PATH = SH_DIRPATH + File.separator + "raw-v2-corrected"
  /** Path to SH directory with annotated dictionaries. */
  val SH_ANOT_PATH = SH_DIRPATH + File.separator + "annotated"
  // Path to the SH directory with original dictionaries (in standard format) having alias frequencies.
//  private val SH_ORIG_FREQS_PATH = SH_DIRPATH + File.separator + "orig-wt-freqs"

  /** Path to SS character list directory. */
  val SS_LIST_DIRPATH = StoryOverseer.getPath(SILVER_STANDARD) + File.separator + "char-lists"
  /** Path to SS character dictionary directory. */
  val SS_DIRPATH = StoryOverseer.getPath(SILVER_STANDARD) + File.separator + DICTS_DIRNAME
  // Path to the SS directory with original dictionaries (in standard format) having alias frequencies.
//  private val SS_ORIG_FREQS_PATH = SH_DIRPATH + File.separator + "orig-wt-freqs"

  /** Path to EL character dictionary directory. */
  val EL_DIRPATH = StoryOverseer.getPath(ELSON) + File.separator + DICTS_DIRNAME

  /** Path to PG character dictionary directory. */
  val PG_DIRPATH = StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + DICTS_DIRNAME

  // Raw or annotated character dictionary CSV indices.
  // Character # index.
  private val CHAR_INDEX = 0
  // Textual span index.
  private val SPAN_INDEX = 1
  // Identity # index.
  private val IDENT_INDEX = 6
  // Alias type index.
  private val TYPE_INDEX = 7
  // Alias sub-type index.
  private val SUBTYPE_INDEX = 8
  // Titled index.
  private val TITLED_INDEX = 9
  // Abbreviated index.
  private val ABBREV_INDEX = 10
  // Qualified index.
  private val QUALIFIED_INDEX = 11

  // Checks if the textual span for a given alias refers to the narrator.
  private def isNarrator(aliasSpan: String): Boolean = aliasSpan.toLowerCase.contains("narrator")

  // Loads the characters from the raw chararacter dictionary .csv file located at the the given filepath.
  private def loadRawCharacters(filepath: String): Set[Character] = {
    var characters: Set[Character] = Set()

    val reader = CSVReader.open(new File(filepath))

    // Skip header.
    reader.readNext()

    var aliases: Set[Alias] = Set()
    var characterIndex = 0
    var readerResult: Option[List[String]] = reader.readNext()
    while(readerResult.nonEmpty) {
      val row: Array[String] = readerResult.get.toArray

      // Skip over aliases containing "narrator" or have an empty character index.
      if (!isNarrator(row(SPAN_INDEX)) && row(CHAR_INDEX).trim != "") {
        if (row(CHAR_INDEX).toInt == characterIndex)
          aliases += new Alias(row(SPAN_INDEX).trim)
        else {
          if (characterIndex > 0)
            characters += Character(aliases)

          aliases = Set(new Alias(row(SPAN_INDEX).trim))
          characterIndex += 1
        }
      }

      readerResult = reader.readNext()
    }

    // Add last character.
    characters += Character(aliases)

    reader.close()

    characters
  }

  // Loads the annotated characters from the annotated character dictionary from the .csv file located at the the given
  // filepath.
  private def loadAnnotatedCharacters(filepath: String): Set[Character] = {
    // Converts the alias-containg CSV row into an Aliasm, complete with annotation.
    def rowToAlias(row: Array[String]): Alias = {
      val span: String = row(SPAN_INDEX).trim
      val identity: Int = row(IDENT_INDEX).toInt

      val aliasType: AliasType = row(TYPE_INDEX).toLowerCase.trim match {
        case "proper" =>
          val properSubType: ProperSubType = row(SUBTYPE_INDEX).toLowerCase.trim match {
            case "full" => FULL
            case "partial" => PARTIAL
            case _ => throw new IllegalArgumentException("Proper alias has unknown subtype " + row(SUBTYPE_INDEX) +
              " in file " + filepath + ".")
          }

          Proper(properSubType)
        case "pseudonym" =>
          val pseudonymSubType: PseudonymSubType = row(SUBTYPE_INDEX).toLowerCase.trim match {
            case "nickname" => NICKNAME
            case "regnal" => REGNAL
            case "virtual" => VIRTUAL
            case _ => throw new IllegalArgumentException("Pseudonym alias has unknown subtype " + row(SUBTYPE_INDEX) +
              " in file " + filepath + ".")
          }

          Pseudonym(pseudonymSubType)
        case "role" =>
          val roleSubType: RoleSubType = row(SUBTYPE_INDEX).toLowerCase.trim match {
            case "kin" => KIN
            case "social" => SOCIAL
            case "job" => JOB
            case _ => throw new IllegalArgumentException("Role alias has unknown subtype " + row(SUBTYPE_INDEX) +
              " in file " + filepath + ".")
          }

          Role(roleSubType)
        case "uga" => UGA
        case "group" =>
          val groupSubType: GroupSubType = row(SUBTYPE_INDEX).toLowerCase.trim match {
            case "organization" => ORGANIZATION
            case "generic" => GENERIC
            case _ => throw new IllegalArgumentException("Group alias has unknown subtype " + row(SUBTYPE_INDEX) +
              " in file " + filepath + ".")
          }

          Group(groupSubType)
        case _ => throw new IllegalArgumentException("Unknown alias type " + row(TYPE_INDEX) + " encountered in file " +
          filepath + ".")
      }

      val titled: Boolean = row(TITLED_INDEX).toLowerCase.trim match {
        case "yes" => true
        case "" => false
        case _ => throw new IllegalArgumentException("Unknown titled entry " + row(TITLED_INDEX) +
          " encountered in file " + filepath + ".")
      }

      val abbrev: Boolean = row(ABBREV_INDEX).toLowerCase.trim match {
        case "yes" => true
        case "" => false
        case _ => throw new IllegalArgumentException("Unknown abbreviated entry " + row(ABBREV_INDEX) +
          " encountered in file " + filepath + ".")
      }

      val qualified: Boolean = row(QUALIFIED_INDEX).toLowerCase.trim match {
        case "yes" => true
        case "" => false
        case _ => throw new IllegalArgumentException("Unknown qualified entry " + row(QUALIFIED_INDEX) +
          " encountered in file " + filepath + ".")
      }

      Alias(span, Some(AliasAnnotation(identity, aliasType, titled, abbrev, qualified)))
    }

    var characters: Set[Character] = Set()

    val reader = CSVReader.open(new File(filepath))

    // Skip header.
    reader.readNext()

    var aliases: Set[Alias] = Set()
    var characterIndex = 0
    var readerResult: Option[List[String]] = reader.readNext()
    while(readerResult.nonEmpty) {
      val row: Array[String] = readerResult.get.toArray

      // Skip over aliases containing "narrator".
      if (!isNarrator(row(SPAN_INDEX))) {
        if (row(CHAR_INDEX).toInt == characterIndex)
          aliases += rowToAlias(row)
        else {
          if (characterIndex > 0)
            characters += Character(aliases)

          aliases = Set(rowToAlias(row))
          characterIndex += 1
        }
      }

      readerResult = reader.readNext()
    }

    reader.close()

    characters
  }

  // Loads character list as a set of characters from the character list text file located at the given filepath (For
  // stories in the  Silver Standard set). Each text file must have each character name listed on its own line,
  // separated from other names only by a CR.
  private def loadCharacterList(filepath: String): Set[Character] =
    fromFile(new File(filepath)).getLines().filterNot(isNarrator).map(s => Character(Set(new Alias(s)))).toSet

  /**
   * Get the character dictionary (annotated or not) for a given story.
   * @param storyId Story Id.
   * @param annotated True if annotated dictionary is desired, false otherwise.
   * @return Character dictionary.
   */
  def getDictionary(storyId: StoryId, annotated: Boolean): CharacterDictionary = storyId match {
    case StoryId(MOONSTONE, None) =>
      if (annotated)
        new CharacterDictionary(storyId, loadAnnotatedCharacters(MS_ANOT_PATH))
      else
        new CharacterDictionary(storyId, loadRawCharacters(MS_RAW_PATH))
    case StoryId(PRIDE_AND_PREJUDICE, None) =>
      if (annotated)
        new CharacterDictionary(storyId, loadAnnotatedCharacters(PP_ANOT_PATH))
      else
        new CharacterDictionary(storyId, loadRawCharacters(PP_RAW_PATH))
    case StoryId(SHERLOCK_HOLMES, Some(name)) =>
      val filepath = (if (annotated) SH_ANOT_PATH else SH_RAW_PATH) + File.separator + name + ".csv"
      if (!new File(filepath).exists)
        throw new IllegalArgumentException("Character dictionary for story Id, " + storyId + ", doesn't exist at " +
          filepath + ".")
      new CharacterDictionary(storyId,
        if (annotated) loadAnnotatedCharacters(filepath) else loadRawCharacters(filepath))
    case StoryId(SILVER_STANDARD, Some(name)) =>
      if (annotated)
        throw new IllegalArgumentException("Annotated versions of Silver Standard set don't exist.")

      val filepath = SS_LIST_DIRPATH + File.separator + name + ".txt"
      if (!new File(filepath).exists)
        throw new IllegalArgumentException("Character list for story Id, " + storyId + ", doesn't exist at " +
          filepath + ".")
      new CharacterDictionary(storyId, loadCharacterList(filepath))
    case _ => throw new StoryNotFoundException(storyId)
  }

  /**
   * Returns the path to the directory containing the character dictionaries for the given story set.
   * @param storyId Story id.
   * @return Directory path as String.
   */
  def getDir(storyId: StoryId): String = storyId match {
    case StoryId(MOONSTONE, None) => MS_DIRPATH
    case StoryId(PRIDE_AND_PREJUDICE, None) => PP_DIRPATH
    case StoryId(SHERLOCK_HOLMES, _) => SH_DIRPATH
    case StoryId(SILVER_STANDARD, _) => SS_DIRPATH
    case StoryId(ELSON, _) => EL_DIRPATH
    case StoryId(PROJECT_GUTENBERG, _) => PG_DIRPATH
    case _ => throw new StoryNotFoundException(storyId)
  }

}

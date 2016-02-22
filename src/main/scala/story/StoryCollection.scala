package story

import java.io.File

import com.github.tototoshi.csv.CSVReader
import stats.Metrics
import story.StorySet._
import text.{TextSplitter, ORIG, TextManager, TextType}


/**
 * Individual story.
 */
trait StoryCollection {

  /**
   * Returns a list of story id's in the collection.
   * @return List of story id's.
   */
  def ids: List[StoryId]

  /**
   * Checks whether the given string can refer to a story in the collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  def isId(s: String): Boolean

  /**
   * Checks whether the given story id refers to a story in the collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the collection, false otherwise.
   */
  def isId(storyId: StoryId): Boolean

  /**
   * Returns the text corresponding to the given story.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  def getText(storyId: StoryId, textType: TextType): String

}

/**
 * Sherlock Holmes collection (SH).
 */
object SherlockHolmes extends StoryCollection {

  // Path to master table .csv file.
  private val SH_MASTER_TABLE: String = StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + "master-table.csv"

  // Thrown when the given story does not belong to the SH collection.
  private class StoryDNEInSHCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in SH collection.")

  /**
   * Returns a list of story id's in the collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    // Collection number column index.
    val colNumIndex = 1
    // Story number column index.
    val storyNumIndex = 3
    // Story title column index.
    val storyTitleIndex = 4

    val reader = CSVReader.open(new File(this.SH_MASTER_TABLE))

    // Skip header row.
    reader.readNext()

    reader.all().map(row => (if (row(colNumIndex).length == 1) "0" else "") + row(colNumIndex) + "_" +
      (if (row(storyNumIndex).length == 1) "0" else "") + row(storyNumIndex) + "_" + row(storyTitleIndex))
                .map(name => StoryId(SHERLOCK_HOLMES, Some(name)))
  }

  /**
   * Checks whether the given string can refer to a story in the collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.contains("_")

  /**
   * Checks whether the given story id correspond to a story in the SH collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the SH collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(SHERLOCK_HOLMES, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the text corresponding to the given story.
   * @param storyId Story id of story.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInSHCollectionException(storyId)
  }

  /**
   * Returns the text for the given story in the collection, as an array of paragraphs.
   * @param storyId Story id of story.
   * @return Array of paragraphs.
   */
  def getParagraphs(storyId: StoryId): Array[String] = storyId match {
    case StoryId(SHERLOCK_HOLMES, Some(name))
      if this.isId(name) && (
          name.startsWith("02") ||
          name.startsWith("04_08") ||
          name.startsWith("05_01") ||
          name.startsWith("05_02") ||
          name.startsWith("05_03") ||
          name.startsWith("05_04") ||
          name.startsWith("05_05")) => TextSplitter.splitParagraphs(getText(storyId, ORIG)).filterNot(_.trim.isEmpty)
    case StoryId(SHERLOCK_HOLMES, Some(name))
      if this.isId(name) && (
          name.startsWith("03_01") ||
          name.startsWith("03_02") ||
          name.startsWith("03_03") ||
          name.startsWith("03_07")) =>
      TextSplitter.splitParagraphs(getText(storyId, ORIG), TextSplitter.SINGLE_CR).filterNot(_.trim.isEmpty)
    case id if this.isId(id) =>
      TextSplitter.splitParagraphs(getText(storyId, ORIG), TextSplitter.SINGLE_LF).filterNot(_.trim.isEmpty)
    case _ => throw new StoryDNEInSHCollectionException(storyId)
  }

}

/**
 * Silver Standard collection (SS).
 */
object SilverStandard extends StoryCollection {

  // Filepath to .csv file containing meta information about the collection.
  private val META_INFO_FILEPATH: String = StoryOverseer.getPath(SILVER_STANDARD) + File.separator + "meta-info.csv"

  // Thrown when the given story does not belong to the SS collection.
  private class StoryDNEInSSCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in SS collection.")

  /**
   * Returns a list of story id's in the SS collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    // Id column index.
    val idIndex = 0

    val reader = CSVReader.open(new File(this.META_INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(_(idIndex)).map(name => StoryId(SILVER_STANDARD, Some(name)))
  }

  /**
   * Checks whether the given string can refer to a story in the SS collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.forall(_.isDigit)

  /**
   * Checks whether the given story id refers to a story in the SS collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the SS collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(SILVER_STANDARD, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the text corresponding to the given story.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInSSCollectionException(storyId)
  }

  /**
   * Returns the text for the given story in the SS collection, as an array of paragraphs.
   * @param storyId Story id of story.
   * @return Array of paragraphs.
   */
  def getParagraphs(storyId: StoryId): Array[String] = storyId match {
    case StoryId(SILVER_STANDARD, Some(_)) =>
      TextSplitter.splitParagraphs(this.getText(storyId, ORIG), TextSplitter.DOUBLE_CRLF).filterNot(_.trim.isEmpty)
    case _ => throw new StoryDNEInSSCollectionException(storyId)
  }

}

/**
 * Elson story collection (EL), from Elson et. al. (2010).
 */
object Elson extends StoryCollection {

  // Filepath to .csv file containing collection information.
  private val INFO_FILEPATH: String = StoryOverseer.getPath(ELSON) + File.separator + "info.csv"

  // Thrown when the given story does not belong to the EL collection.
  private class StoryDNEInELCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in EL collection.")

  /**
   * Returns a list of story id's in the EL collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    val ssIds: Set[String] = SilverStandard.ids.map(_.storyName.get).toSet
    val pgIds: Set[String] = ProjectGutenberg.ids.map(_.storyName.get).toSet

    CSVReader
      .open(new File(this.INFO_FILEPATH))
      .all()
      .flatMap(_.last.split(",").map(_.trim))
      .toSet
      .diff(ssIds | pgIds)
      .map(id => StoryId(ELSON, Some(id)))
      .toList
  }

  /**
   * Checks whether the given string can refer to a story in the EL collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.forall(_.isDigit)

  /**
   * Checks whether the given story id refers to a story in the EL collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the EL collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(ELSON, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the text corresponding to the given story in the EL collection.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInELCollectionException(storyId)
  }

}

/**
 * Project Gutenberg collection (PG).
 */
object ProjectGutenberg extends StoryCollection {

  // Filepath to .csv file containing story ids.
  private val IDS_FILEPATH: String = StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + "ids.txt"

  // Thrown when the given story does not belong to the PG collection.
  private class StoryDNEInPGCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in PG collection.")

  /**
   * Returns a list of story id's in the PG collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    val ssIds: Set[String] = SilverStandard.ids.map(_.storyName.get).toSet

    io.Source.fromFile(new File(this.IDS_FILEPATH))
      .getLines()
      .toSet
      .diff(ssIds)
      .map(id => StoryId(PROJECT_GUTENBERG, Some(id.toString)))
      .toList
  }

  /**
   * Checks whether the given string can refer to a story in the PG collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.forall(_.isDigit)

  /**
   * Checks whether the given story id refers to a story in the PG collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the PG collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(PROJECT_GUTENBERG, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the text corresponding to the given story in the PG collection.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInPGCollectionException(storyId)
  }

}

/**
 * Piper's collection of 10,070 stories (PIP).
 */
object Piper extends StoryCollection {

  // Filepath to .csv file containing collection information.
  private val INFO_FILEPATH: String = StoryOverseer.getPath(PIPER) + File.separator + "info.csv"

  // Indices in collection information .csv file, where each row corresponds to a story.
  // Id.
  private val INFO_ID_INDEX = 0
  // World catalogue index (i.e. # libraries possessing the story).
  private val INFO_LIBRARIES_INDEX = 1
  // Title.
  private val INFO_TITLE_INDEX = 2
  // Author's first name.
  private val INFO_AUTHOR_FIRST_INDEX = 4
  // Author's last name.
  private val INFO_AUTHOR_LAST_INDEX = 3
  // Publication year.
  private val INFO_PUB_YEAR_INDEX = 8
  // Word count.
  private val INFO_WORD_COUNT_INDEX = 12

  // Thrown when the given story does not belong to the PIP collection.
  private class StoryDNEInPIPCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in PIP collection.")

  /**
   * Returns a list of story id's in the PIP collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    val reader = CSVReader.open(new File(this.INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(_(this.INFO_ID_INDEX)).map(id => {
      // Name must have length 8, achieved by padding with initial '0's.
      val name: String = (1 to 8 - id.length).map(i => "0").mkString("") + id
      StoryId(PIPER, Some(name))
    })
  }

  /**
   * Checks whether the given string can refer to a story in the PIP collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.forall(_.isDigit)

  /**
   * Checks whether the given story id refers to a story in the PIP collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the PIP collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(PIPER, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the PIP story object for the given story in the collection.
   * @param storyId Story id of story.
   * @return Piper story object.
   */
  def getStory(storyId: StoryId): PiperStory = storyId match {
    case StoryId(PIPER, Some(name)) =>
      val reader = CSVReader.open(new File(this.INFO_FILEPATH))

      // Skip header row.
      reader.readNext()

      reader.iterator.find(_(this.INFO_ID_INDEX).toInt == name.toInt) match {
        case Some(row) => PiperStory(
            storyId,
            row(this.INFO_TITLE_INDEX),
            row(this.INFO_AUTHOR_FIRST_INDEX),
            row(this.INFO_AUTHOR_LAST_INDEX),
            row(this.INFO_LIBRARIES_INDEX).toInt,
            row(this.INFO_PUB_YEAR_INDEX).toInt,
            row(this.INFO_WORD_COUNT_INDEX).toInt
          )
        case _ => throw new StoryDNEInPIPCollectionException(storyId)
      }
    case _ => throw new StoryDNEInPIPCollectionException(storyId)
  }

  /**
   * Returns a mapping from story id to Piper story object.
   * @return Mapping from story id to Piper story object.
   */
  def storyMap: Map[StoryId, PiperStory] = {
    val reader = CSVReader.open(new File(this.INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(row => {
      // Name must have length 8, achieved by padding with initial '0's.
      val name: String = (1 to 8 - row(this.INFO_ID_INDEX).length).map(i => "0").mkString("") + row(this.INFO_ID_INDEX)
      val storyId: StoryId = StoryId(PIPER, Some(name))
      storyId -> PiperStory(
          storyId,
          row(this.INFO_TITLE_INDEX),
          row(this.INFO_AUTHOR_FIRST_INDEX),
          row(this.INFO_AUTHOR_LAST_INDEX),
          row(this.INFO_LIBRARIES_INDEX).toInt,
          row(this.INFO_PUB_YEAR_INDEX).toInt,
          row(this.INFO_WORD_COUNT_INDEX).toInt
        )
    }).toMap
  }

  /**
   * Returns the text corresponding to the given story in the PIP collection.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInPIPCollectionException(storyId)
  }

}

/**
 * Wilkens' story collection (WK).
 */
object Wilkens extends StoryCollection {

  // Filepath to .csv file containing collection information.
  private val INFO_FILEPATH: String = StoryOverseer.getPath(WILKENS) + File.separator + "info.csv"

  // Indices in collection information .csv file, where each row corresponds to a story.
  // Id.
  private val INFO_ID_INDEX = 0
  // Title.
  private val INFO_TITLE_INDEX = 3
  // Author's first name.
  private val INFO_AUTHOR_FIRST_INDEX = 5
  // Author's last name.
  private val INFO_AUTHOR_LAST_INDEX = 4
  // Publication year.
  private val INFO_PUB_YEAR_INDEX = 9
  // Word count.
  private val INFO_WORD_COUNT_INDEX = 22

  // Thrown when the given story does not belong to the WK collection.
  private class StoryDNEInWKCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in WK collection.")

  /**
   * Returns a list of story id's in the WK collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    val reader = CSVReader.open(new File(this.INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(_(this.INFO_ID_INDEX)).map(name => StoryId(WILKENS, Some(name)))
  }

  /**
   * Checks whether the given string can refer to a story in the WK collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = s.startsWith("eaf") || s.startsWith("Wright2-")

  /**
   * Checks whether the given story id refers to a story in the WK collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the WK collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(WILKENS, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the WK story object for the given story in the collection.
   * @param storyId Story id of story.
   * @return Wilkens story object.
   */
  def getStory(storyId: StoryId): WilkensStory = storyId match {
    case StoryId(WILKENS, Some(name)) =>
      val reader = CSVReader.open(new File(this.INFO_FILEPATH))

      // Skip header row.
      reader.readNext()

      reader.iterator.find(_(this.INFO_ID_INDEX) == name) match {
        case Some(row) => WilkensStory(
          storyId,
          row(this.INFO_TITLE_INDEX),
          row(this.INFO_AUTHOR_FIRST_INDEX),
          row(this.INFO_AUTHOR_LAST_INDEX),
          row(this.INFO_PUB_YEAR_INDEX).toInt,
          row(this.INFO_WORD_COUNT_INDEX).toInt
        )
        case _ => throw new StoryDNEInWKCollectionException(storyId)
      }
    case _ => throw new StoryDNEInWKCollectionException(storyId)
  }

  /**
   * Returns a mapping from story id to Wilkens story object.
   * @return Mapping from story id to Wilkens story object.
   */
  def storyMap: Map[StoryId, WilkensStory] = {
    val reader = CSVReader.open(new File(this.INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(row => {
      val storyId: StoryId = StoryId(WILKENS, Some(row(this.INFO_ID_INDEX)))
      storyId -> WilkensStory(
        storyId,
        row(this.INFO_TITLE_INDEX),
        row(this.INFO_AUTHOR_FIRST_INDEX),
        row(this.INFO_AUTHOR_LAST_INDEX),
        row(this.INFO_PUB_YEAR_INDEX).toInt,
        row(this.INFO_WORD_COUNT_INDEX).toInt
      )
    }).toMap
  }

  /**
   * Returns the text corresponding to the given story in the WK collection.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInWKCollectionException(storyId)
  }

}

/**
 * Scribner short story collection (SC).
 */
object Scribner extends StoryCollection {

  // Filepath to .csv file containing information about the collection.
  private val INFO_FILEPATH: String = StoryOverseer.getPath(SCRIBNER) + File.separator + "selected-info.csv"

  // Thrown when the given story does not belong to the SS collection.
  private class StoryDNEInSCCollectionException(storyId: StoryId) extends RuntimeException("Story " + storyId +
    " DNE in SC collection.")

  /**
   * Returns a list of story id's in the SC collection.
   * @return List of story id's.
   */
  override def ids: List[StoryId] = {
    // Id column index.
    val idIndex = 0

    val reader = CSVReader.open(new File(this.INFO_FILEPATH))

    // Skip header row.
    reader.readNext()

    reader.all().map(_(idIndex)).map(name => StoryId(SCRIBNER, Some(name)))
  }

  /**
   * Checks whether the given string refers to a story in the SC collection.
   * @param s String.
   * @return True if it can, false otherwise.
   */
  override def isId(s: String): Boolean = this.ids.exists(_.storyName.get == s)

  /**
   * Checks whether the given story id refers to a story in the SC collection.
   * @param storyId Story id of story.
   * @return True if the story id corresponds to a story in the SC collection, false otherwise.
   */
  override def isId(storyId: StoryId): Boolean = storyId match {
    case StoryId(SCRIBNER, Some(name)) => this.isId(name)
    case _ => false
  }

  /**
   * Returns the text corresponding to the given story.
   * @param storyId Story id of story.
   * @param textType Text type.
   * @return Story text.
   */
  override def getText(storyId: StoryId, textType: TextType): String = storyId match {
    case id if this.isId(id) => TextManager.getText(storyId, textType)
    case _ => throw new StoryDNEInSCCollectionException(storyId)
  }

  /**
   * Returns the text for the given story in the SC collection, as an array of paragraphs.
   * @param storyId Story id of story.
   * @return Array of paragraphs.
   */
  def getParagraphs(storyId: StoryId): Array[String] = storyId match {
    case StoryId(SCRIBNER, Some(_)) =>
      TextSplitter.splitParagraphs(this.getText(storyId, ORIG), TextSplitter.DOUBLE_CRLF).filterNot(_.trim.isEmpty)
    case _ => throw new StoryDNEInSCCollectionException(storyId)
  }

}

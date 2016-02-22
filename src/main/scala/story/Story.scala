package story

import text.{ORIG, TextSplitter, TextManager, TextType}


/**
 * Individual story.
 */
trait Story {

  /** Story id. */
  val id: StoryId

  /**
   * Returns the text of the story according to the given text type.
   * @param textType Text type.
   * @return Story text.
   */
  def getText(textType: TextType): String = TextManager.getText(this.id, textType)

}

/**
 * Pride and Prejudice story container.
 */
object PrideAndPrejudice extends Story {

  /** Story Id. */
  val id = new StoryId(StorySet.PRIDE_AND_PREJUDICE)

  /** Total number of chapters. */
  val numChapters = 61

  // Signifies a new chapter in the text.
  private val chapterSignifer = "Chapter"

  /**
   * Splits the text according to the chapters.
   * @return Array of chapter texts.
   */
  def splitChapters: Array[String] =
    this.getText(ORIG).split(this.chapterSignifer + " \\d+").map(_.trim).filterNot(_.isEmpty)

  /**
   * Returns the chapter text for the given chapter.
   * @param chapterNumber Chapter number.
   * @return Chapter text.
   */
  def getChapter(chapterNumber: Int): String = {
    if (chapterNumber < 1 || chapterNumber > this.numChapters)
      throw new IllegalArgumentException("Chapter number must be between 1 and " + this.numChapters + ".")

    this.splitChapters(chapterNumber - 1)
  }

  /**
   * Returns the chapter paragraphs for the given chapter.
   * @param chapterNumber Chapter number.
   * @return Array of paragraphs.
   */
  def getChapterParagraphs(chapterNumber: Int): Array[String] =
    TextSplitter.splitParagraphs(this.getChapter(chapterNumber))

  /**
   * Returns the paragraphs.
   * @return Array of paragraphs.
   */
  def getParagraphs: Array[String] = (1 to this.numChapters).flatMap(this.getChapterParagraphs).toArray

}

/**
 * Container for a single Piper story.
 * @param id Story id of story.
 * @param title Title.
 * @param authorFirstName Author's first name.
 * @param authorLastName Author's last name.
 * @param worldCatalogueIndex World catalogue index (i.e. # libraries possessing the story).
 * @param year Publication year.
 * @param wordCount Word count.
 */
case class PiperStory(override val id: StoryId,
                      title: String,
                      authorFirstName: String,
                      authorLastName: String,
                      worldCatalogueIndex: Int,
                      year: Int,
                      wordCount: Int) extends Story

/**
 * Container for a single Wilkens story.
 * @param id Story id of story.
 * @param title Title.
 * @param authorFirstName Author's first name.
 * @param authorLastName Author's last name.
 * @param year Publication year.
 * @param wordCount Word count.
 */
case class WilkensStory(override val id: StoryId,
                      title: String,
                      authorFirstName: String,
                      authorLastName: String,
                      year: Int,
                      wordCount: Int) extends Story

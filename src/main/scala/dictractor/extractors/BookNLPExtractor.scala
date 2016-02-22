package dictractor.extractors

import java.io.File

import dict.{Alias, Character, CharacterDictionary}
import novels.BookNLP
import org.apache.commons.io.FileUtils
import story.StoryId
import story.StorySet._
import text.{ORIG, TextManager}


/**
 * BookNLP character dictionary extractor.
 */
object BookNLPExtractor extends DictExtractor {

  /** BookNLP cached dictionary filename (for single stories) stored in standard .csv format (without alias counts). */
  override protected val cacheDictFilename: String = "book-nlp.csv"
  /** BookNLP cached dictionary filename (for single stories) stored in standard .csv format (with alias counts). */
  override protected val cacheDictWithCountsFilename: String = "book-nlp-wt-cnts.csv"
  /** BookNLP cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (without alias counts). */
  override protected val cacheDictDirname: String = "book-nlp"
  /** BookNLP cached dictionary directory name for story collections with dictionaries stored in standard .csv format
    * (with alias counts). */
  override protected val cacheDictWithCountsDirname: String = "book-nlp-wt-cnts"

  // Run BookNLP with the text specified by the input filepath and output saved to the given output directory path.
  private def run(inputPath: String, outputDirPath: String): Unit =
    BookNLP.main(Array("-doc", inputPath, "-printHTML", "-p", outputDirPath, "-tok",
      outputDirPath + File.separator + "tokens", "-f"))

  // Retrieve the temporary directory.
  private def tmpDir: File = {
    val td = new File(".tmp")
    if (!td.exists())
      td.mkdirs()
    td
  }

  // Cleans up the temporary directory by deleting it and its contents.
  private def cleanUpTmpDir: Unit = {
    if (this.tmpDir.exists())
      FileUtils.deleteDirectory(this.tmpDir)
  }

  // Parses the .html output file produced by BookNLP into a set of characters.
  private def parseHTMLFile(filepath: String): Set[Character] = {
    val firstLine: String = io.Source.fromFile(new File(filepath)).getLines().next()
    val characterDictionaryContent: String = firstLine.substring(firstLine.indexOf("</h1>") + 5,
      firstLine.lastIndexOf("<h1>"))
    val characterContents: Array[String] = characterDictionaryContent.split("<br />")

    characterContents.map(c =>
      Character(c.replaceAll("\\d+", "")
        .split("\\(\\)")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .map(a => new Alias(a)).toSet)).toSet
  }

  /**
   * Extracts the BookNLP character dictionary for the given story.
   * @param storyId Story id of story.
   * @param withCounts Whether to include alias counts in the extracted dictionary.
   * @return BookNLP extracted character dictionary.
   */
  override def extract(storyId: StoryId, withCounts: Boolean): CharacterDictionary = {
    if (this.inCache(storyId, withCounts))
      this.fromCache(storyId, withCounts)
    else if (withCounts && this.inCache(storyId, false)) {
      val cd: CharacterDictionary = this.fromCache(storyId, false)
      cd.getAliasCounts
      this.toCache((cd.storyId, true), cd)
      cd
    } else {
      try {
        this.run(TextManager.getTextFile(storyId, ORIG).getPath, this.tmpDir.getPath)
        val cd = new CharacterDictionary(storyId,
          this.parseHTMLFile(this.tmpDir.getPath + File.separator + "book.id.html"))
        // Save the character dictionary to cache.
        this.toCache((cd.storyId, withCounts), cd)
        cd
      } finally {
        this.cleanUpTmpDir
      }
    }
  }

}

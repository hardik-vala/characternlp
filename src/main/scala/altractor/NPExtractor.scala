package altractor

import java.io.File

import dict.Alias
import name.NameTitles
import story.{StoryNotFoundException, StoryId}
import story.StorySet._
import text.{NERText, NER_WT_TITLES, TOK, TextManager}

import scala.util.matching.Regex


trait NPExtractor extends AliasExtractor {

  /** Name for folder containing cachec extracted alaises for NP-based methods. */
  protected val NP_DIRNAME = "np"

}

/**
 * Extracts the raw named persons according to Stanford's NER.
 */
object RawNPExtractor extends NPExtractor {

  /** Filename for cached extracted raw named persons (Applies to single stories). */
  override protected val cacheAliasesFilename: String = this.NP_DIRNAME + File.separator + "raw.csv"
  /** Directory name for cached extracted raw named persons (Applies to story collections). */
  override protected val cacheAliasesDirname: String = this.NP_DIRNAME + File.separator + "raw"

  /**
   * Extracts the set of raw named persons in a given story.
   * @param storyId Story id of story.
   * @return Set of raw named persons.
   */
  override def extract(storyId: StoryId): Set[_ <: Alias] = this.fromCache(storyId).asInstanceOf[Set[Alias]]

}

/**
 * Extracts the named persons, according to Stanford's NER, with any name titles included.
 */
object NPWithTitleExtractor extends NPExtractor {

  /** Filename for cached extracted named persons with titles (Applies to single stories). */
  override protected val cacheAliasesFilename: String = this.NP_DIRNAME + File.separator + "wt-title.csv"
  /** Directory name for cached extracted named persons with titles (Applies to story collections). */
  override protected val cacheAliasesDirname: String = this.NP_DIRNAME + File.separator + "wt-title"

  /**
   * Extracts the set of named persons with titles in a given story.
   * @param storyId Story id of story.
   * @return Set of named persons with titles.
   */
  override def extract(storyId: StoryId): Set[_ <: Alias] = this.fromCache(storyId).asInstanceOf[Set[Alias]]

}

/**
 * Filters only the strong NP's from [[altractor.NPWithTitleExtractor]], i.e. those that have the majority of
 * occurrences assigned PERSON in the particular NER annotated text.
 */
object StrongNPWithTitleExtractor extends NPExtractor {

  /** Filename for cached extracted strong named persons with titles (Applies to single stories). */
  override protected val cacheAliasesFilename: String = this.NP_DIRNAME + File.separator + "strong-wt-title.csv"
  /** Directory name for cached extracted strong named persons with titles (Applies to story collections). */
  override protected val cacheAliasesDirname: String = this.NP_DIRNAME + File.separator + "strong-wt-title"

  // Checks whether the given string is a strong NP according to the given NER annotated text.
  private def isStrongNP(s: String, nerAnnotatedText: String): Boolean = NERText.isPerson(s, nerAnnotatedText)

  // Filters the strong NP's from the givn set of NP's (with titles) according to the given NER annotated text.
  private def filterStrongNPs(npsWithTitles: Set[_ <: Alias], nerAnnotatedText: String): Set[_ <: Alias] =
    npsWithTitles.filter(a => this.isStrongNP(a.span, nerAnnotatedText))

  /**
   * Extracts the Strong NP's for the given story.
   * @param storyId Story id of story.
   * @return Set of strong NP's.
   */
  override def extract(storyId: StoryId): Set[_ <: Alias] = {
    if (this.inCache(storyId))
      this.fromCache(storyId).asInstanceOf[Set[Alias]]
    else {
      val npsWithTitle: Set[_ <: Alias] = NPWithTitleExtractor.extract(storyId)
      val nerAnnotatedText = TextManager.getText(storyId, NER_WT_TITLES)
      val aliases = this.filterStrongNPs(npsWithTitle, nerAnnotatedText)
      this.toCache(storyId, aliases)
      aliases
    }
  }

}


/**
 * Unions the output of [[altractor.StrongNPWithTitleExtractor]] with any missed titled names.
 */
object NPPlusTitledExtractor extends NPExtractor {

  /** Filename for cached extracted named persons with titles, plus titled names (Applies to single stories). */
  override protected val cacheAliasesFilename: String = this.NP_DIRNAME + File.separator + "plus-titled.csv"
  /** Directory name for cached extracted named persons with titles, plus titled names (Applies to story
    * collections). */
  override protected val cacheAliasesDirname: String = this.NP_DIRNAME + File.separator + "plus-titled"

  /**
   * Extracts the titled names from the given tokenized text.
   * @param tokenizedText Tokenized text.
   * @return Set of titled names.
   */
  def extractTitledNames(tokenizedText: String): Set[Alias] =
    NameTitles.get.flatMap(t => {
      val pattern = new Regex("(^|\\s)(" + t + "|" + t.toUpperCase + ") (([A-Z][\\w-]+ )|([A-Z]\\.))+($|\\s)")
      // Alternate pattern includes titled names that have a middle name that's not capitalized.
//      ") ((([A-Z][\\w-]+ )|([A-Z]\\.))+|([A-Z]\\w+ [a-z]\\w+ [A-Z]\\w+))\\s")
      pattern.findAllIn(tokenizedText).filterNot(_.matches("\\W+")).map(s => new Alias(s.trim)).toSet
    })

  /**
   * Extracts the set of strong NP's from the given story, along with any missed titled names.
   * @param storyId Story id of story.
   * @return Set of strong NP's and missed titled names.
   */
  override def extract(storyId: StoryId): Set[_ <: Alias] = {
    if (this.inCache(storyId))
      this.fromCache(storyId).asInstanceOf[Set[Alias]]
    else {
      val aliases: Set[_ <: Alias] =
        this.extractTitledNames(TextManager.getText(storyId, TOK)) | StrongNPWithTitleExtractor.extract(storyId).toSet
      this.toCache(storyId, aliases)
      aliases
    }
  }

}

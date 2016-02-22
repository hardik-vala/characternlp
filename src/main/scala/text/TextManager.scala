package text

import io.Source._

import java.io.{FileWriter, FileNotFoundException, File}

import story.{StoryNotFoundException, StoryId, StoryOverseer}
import story.StorySet._


/**
 * Manages the different texts for The Moonstone (MS), Pride and Prejudice (PP), the Sherlock Holmes (SH) collection,
 * and the Silver Standard collection.
 */
object TextManager {

  private val TEXT_DIRNAME = "texts"

  /** Path to EL text directory. */
  val EL_DIRPATH = StoryOverseer.getPath(ELSON) + File.separator + TEXT_DIRNAME
  /** Path to EL directory with original texts. */
  val EL_ORIG_PATH = EL_DIRPATH + File.separator + "orig"
  /** Path to EL directory with tokenized texts. */
  val EL_TOK_PATH = EL_DIRPATH + File.separator + "tokenized"
  /** Path to EL directory with NER annotated texts. */
  val EL_NER_PATH = EL_DIRPATH + File.separator + "ner-annotated"
  /** Path to EL directory with NER (with titles) annotated texts. */
  val EL_NER_WT_TITLES_PATH = EL_DIRPATH + File.separator + "ner-annotated-wt-titles"

  /** Path to MS text directory. */
  val MS_DIRPATH = StoryOverseer.getPath(MOONSTONE) + File.separator + TEXT_DIRNAME
  /** Path to original MS text. */
  val MS_ORIG_PATH = MS_DIRPATH + File.separator + "orig.txt"
  /** Path to tokenized MS text. */
  val MS_TOK_PATH = MS_DIRPATH + File.separator + "tokenized.txt"
  /** Path to NER annotated MS text. */
  val MS_NER_PATH = MS_DIRPATH + File.separator + "ner-annotated.txt"
  /** Path to NER (with titles) annotated MS text. */
  val MS_NER_WT_TITLES_PATH = MS_DIRPATH + File.separator + "ner-annotated-wt-titles.txt"

  /** Path to PIP text directory. */
  val PIP_DIRPATH = StoryOverseer.getPath(PIPER) + File.separator + TEXT_DIRNAME
  /** Path to PIP directory with original texts. */
  val PIP_ORIG_PATH = PIP_DIRPATH + File.separator + "orig"
  /** Path to PIP directory with tokenized texts. */
  val PIP_TOK_PATH = PIP_DIRPATH + File.separator + "tokenized"

  /** Path to PP text directory. */
  val PP_DIRPATH = StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + TEXT_DIRNAME
  /** Path to original PP text. */
  val PP_ORIG_PATH = PP_DIRPATH + File.separator + "orig.txt"
  /** Path to stripped PP text. */
  val PP_STRIPPED_PATH = PP_DIRPATH + File.separator + "stripped.txt"
  /** Path to tokenized PP text. */
  val PP_TOK_PATH = PP_DIRPATH + File.separator + "tokenized.txt"
  /** Path to NER annotated PP text. */
  val PP_NER_PATH = PP_DIRPATH + File.separator + "ner-annotated.txt"
  /** Path to NER (with titles) annotated PP text. */
  val PP_NER_WT_TITLES_PATH = PP_DIRPATH + File.separator + "ner-annotated-wt-titles.txt"

  /** Path to PG text directory. */
  val PG_DIRPATH = StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + TEXT_DIRNAME
  /** Path to PG directory with original texts. */
  val PG_ORIG_PATH = PG_DIRPATH + File.separator + "orig"
  /** Path to PG directory with tokenized texts. */
  val PG_TOK_PATH = PG_DIRPATH + File.separator + "tokenized"
  /** Path to PG directory with NER annotated texts. */
  val PG_NER_PATH = PG_DIRPATH + File.separator + "ner-annotated"
  /** Path to PG directory with NER (with titles) annotated texts. */
  val PG_NER_WT_TITLES_PATH = PG_DIRPATH + File.separator + "ner-annotated-wt-titles"

  /** Path to SC text directory. */
  val SC_DIRPATH = StoryOverseer.getPath(SCRIBNER) + File.separator + TEXT_DIRNAME
  /** Path to SC directory with original texts. */
  val SC_ORIG_PATH = SC_DIRPATH + File.separator + "orig"
  /** Path to SC directory with tokenized texts. */
  val SC_TOK_PATH = SC_DIRPATH + File.separator + "tokenized"

  /** Path to SH text directory. */
  val SH_DIRPATH = StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + TEXT_DIRNAME
  /** Path to SH directory with original texts. */
  val SH_ORIG_PATH = SH_DIRPATH + File.separator + "orig"
  /** Path to SH directory with tokenized texts. */
  val SH_TOK_PATH = SH_DIRPATH + File.separator + "tokenized"
  /** Path to SH directory with NER annotated texts. */
  val SH_NER_PATH = SH_DIRPATH + File.separator + "ner-annotated"
  /** Path to SH directory with NER (with titles) annotated texts. */
  val SH_NER_WT_TITLES_PATH = SH_DIRPATH + File.separator + "ner-annotated-wt-titles"

  /** Path to SS text directory. */
  val SS_DIRPATH = StoryOverseer.getPath(SILVER_STANDARD) + File.separator + TEXT_DIRNAME
  /** Path to SS directory with original texts. */
  val SS_ORIG_PATH = SS_DIRPATH + File.separator + "orig"
  /** Path to SS directory with tokenized texts. */
  val SS_TOK_PATH = SS_DIRPATH + File.separator + "tokenized"
  /** Path to SS directory with NER annotated texts. */
  val SS_NER_PATH = SS_DIRPATH + File.separator + "ner-annotated"
  /** Path to SS directory with NER (with titles) annotated texts. */
  val SS_NER_WT_TITLES_PATH = SS_DIRPATH + File.separator + "ner-annotated-wt-titles"

  /** Path to WK text directory. */
  val WK_DIRPATH = StoryOverseer.getPath(WILKENS) + File.separator + TEXT_DIRNAME
  /** Path to WK directory with original texts. */
  val WK_ORIG_PATH = WK_DIRPATH + File.separator + "orig"
  /** Path to WK directory with tokenized texts. */
  val WK_TOK_PATH = WK_DIRPATH + File.separator + "tokenized"

  // Retrives the story's original text file for the given story according to the specified text type.
  private def getOriginalTextFile(storyId: StoryId):File = {
    def defaultGetOriginalTextFileFromCollection(storyId: StoryId): File = {
      val filepath = storyId match {
        case StoryId(ELSON, Some(name)) => EL_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(PIPER, Some(name)) => PIP_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(PROJECT_GUTENBERG, Some(name)) => PG_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(SCRIBNER, Some(name)) => SC_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(SHERLOCK_HOLMES, Some(name)) => SH_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(SILVER_STANDARD, Some(name)) => SS_ORIG_PATH + File.separator + name + ".txt"
        case StoryId(WILKENS, Some(name)) => WK_ORIG_PATH + File.separator + name + ".txt"
      }

      val f = new File(filepath)
      if (!f.exists)
        throw new StoryNotFoundException(storyId)
      f
    }

    storyId match {
      case StoryId(MOONSTONE, None) => new File(MS_ORIG_PATH)
      case StoryId(PRIDE_AND_PREJUDICE, None) => new File(PP_ORIG_PATH)
      case StoryId(ELSON, Some(_)) |
           StoryId(PIPER, Some(_)) |
           StoryId(PROJECT_GUTENBERG, Some(_)) |
           StoryId(SCRIBNER, Some(_)) |
           StoryId(SHERLOCK_HOLMES, Some(_)) |
           StoryId(SILVER_STANDARD, Some(_)) |
           StoryId(WILKENS, Some(_)) => defaultGetOriginalTextFileFromCollection(storyId)
      case _ => throw new StoryNotFoundException(storyId)
    }
  }

  /**
   * Retrieves the story text file for the given story according to the specified text type.
   *
   * @param storyId Story id.
   * @param textType Text type.
   * @return Story text file according to the text type.
   */
  def getTextFile(storyId: StoryId, textType: TextType):File = {
    // Retrieves the tokenized text file located at the given filepath.
    def toTokenizedTextFile(filepath: String):File = {
      val tokenizedTextFile = new File(filepath)
      if (tokenizedTextFile.exists)
        tokenizedTextFile
      else {
        val fw = new FileWriter(filepath)
        try {
          fw.write(Tokenizer.tokenize(getOriginalTextFile(storyId)))
        } catch {
          case e: Exception => throw e
        } finally {
          fw.close()
        }

        tokenizedTextFile
      }
    }

    def defaultGetTokenizedFileFromCollection(storyId: StoryId): File = {
      val dirpath = storyId match {
        case StoryId(ELSON, Some(_)) => EL_TOK_PATH
        case StoryId(PIPER, Some(_)) => PIP_TOK_PATH
        case StoryId(PROJECT_GUTENBERG, Some(_)) => PG_TOK_PATH
        case StoryId(SCRIBNER, Some(_)) => SC_TOK_PATH
        case StoryId(SHERLOCK_HOLMES, Some(_)) => SH_TOK_PATH
        case StoryId(SILVER_STANDARD, Some(_)) => SS_TOK_PATH
        case StoryId(WILKENS, Some(_)) => WK_TOK_PATH
      }

      if (!getOriginalTextFile(storyId).exists)
        throw new StoryNotFoundException(storyId)

      // Create the directory if it doesn't already exist.
      new File(dirpath).mkdirs()

      toTokenizedTextFile(dirpath + File.separator + storyId.storyName.get + ".txt")
    }

    textType match {
      case ORIG => getOriginalTextFile(storyId)
      case STRIPPED => storyId match {
        case StoryId(PRIDE_AND_PREJUDICE, None) => new File(PP_STRIPPED_PATH)
        case StoryId(ELSON, Some(_)) |
             StoryId(MOONSTONE, None) |
             StoryId(PIPER, Some(_)) |
             StoryId(PROJECT_GUTENBERG, Some(_)) |
             StoryId(SCRIBNER, Some(_)) |
             StoryId(SHERLOCK_HOLMES, Some(_)) |
             StoryId(SILVER_STANDARD, Some(_)) |
             StoryId(WILKENS, Some(_)) => throw new NotImplementedError
        case _ => throw new StoryNotFoundException(storyId)
      }
      case TOK => storyId match {
        case StoryId(MOONSTONE, None) => toTokenizedTextFile(MS_TOK_PATH)
        case StoryId(PRIDE_AND_PREJUDICE, None) => toTokenizedTextFile(PP_TOK_PATH)
        case StoryId(ELSON, Some(_)) |
             StoryId(PIPER, Some(_)) |
             StoryId(PROJECT_GUTENBERG, Some(_)) |
             StoryId(SCRIBNER, Some(_)) |
             StoryId(SHERLOCK_HOLMES, Some(_)) |
             StoryId(SILVER_STANDARD, Some(_)) |
             StoryId(WILKENS, Some(_)) => defaultGetTokenizedFileFromCollection(storyId)
        case _ => throw new StoryNotFoundException(storyId)
      }
      case NER => storyId match {
        case StoryId(MOONSTONE, None) => new File(MS_NER_PATH)
        case StoryId(PRIDE_AND_PREJUDICE, None) => new File(PP_NER_PATH)
        case StoryId(SHERLOCK_HOLMES, Some(name)) =>
          val f = new File(SH_NER_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(SILVER_STANDARD, Some(name)) =>
          val f = new File(SS_NER_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(ELSON, Some(name)) =>
          val f = new File(EL_NER_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(PROJECT_GUTENBERG, Some(name)) =>
          val f = new File(PP_NER_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(SCRIBNER, Some(_)) => throw new NotImplementedError
        case _ => throw new StoryNotFoundException(storyId)
      }
      case NER_WT_TITLES => storyId match {
        case StoryId(MOONSTONE, None) => new File(MS_NER_WT_TITLES_PATH)
        case StoryId(PRIDE_AND_PREJUDICE, None) => new File(PP_NER_WT_TITLES_PATH)
        case StoryId(SHERLOCK_HOLMES, Some(name)) =>
          val f = new File(SH_NER_WT_TITLES_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(SILVER_STANDARD, Some(name)) =>
          val f = new File(SS_NER_WT_TITLES_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(ELSON, Some(name)) =>
          val f = new File(EL_NER_WT_TITLES_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(PROJECT_GUTENBERG, Some(name)) =>
          val f = new File(PG_NER_WT_TITLES_PATH + File.separator + name + ".txt")
          if (!f.exists)
            throw new StoryNotFoundException(storyId)
          f
        case StoryId(SCRIBNER, Some(_)) => throw new NotImplementedError
        case _ => throw new StoryNotFoundException(storyId)
      }
    }
  }

  /**
   * Retrieves the story text for the given story according to the specified text type.
   *
   * @param storyId Story id.
   * @param textType Text type.
   * @return Story text according to the text type.
   */
  def getText(storyId: StoryId, textType: TextType):String = fromFile(getTextFile(storyId, textType).getPath).mkString

}

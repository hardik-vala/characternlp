package dictractor.coref

import java.io.File
import java.util.Properties

import edu.stanford.nlp.dcoref.CorefChain.CorefMention
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.dcoref.{CorefChain, Dictionaries}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import name._
import story.StorySet._
import story._

import scala.collection.JavaConversions._


/**
 * Wrapper for Stanford's CoreNLP coreference resolution system.
 */
object RawCorefResoluter extends CorefResoluter {

  /** Resoluter id. */
  override val id: String = "raw"

  // Properties for Stanford CoreNLP pipeline.
  private val props: Properties = new Properties()
  props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")

  // Stanford CoreNLP pipeline.
  private lazy val pipeline: StanfordCoreNLP = new StanfordCoreNLP(this.props)

  /** Cache filename for the coref. output of a single story. */
  override protected val cacheStoryFilename = this.id + ".csv"
  /** Cache directory name for coref. output of the texts in a story collection. */
  override protected val cacheStoryDirname = this.id

  // Checks whether the given coref. mention is animate.
  private def isAnimate(m: CorefMention): Boolean = m.animacy == Dictionaries.Animacy.ANIMATE

  // Checks whether the given coref. mention is pronomial.
  private def isPronomial(m: CorefMention): Boolean = m.mentionType == Dictionaries.MentionType.PRONOMINAL

  // Checks whether the given coref. mention is list (whatever that is).
  private def isList(m: CorefMention): Boolean = m.mentionType == Dictionaries.MentionType.LIST

  // Filters the direct coref. output of chains with representative mentions that are animate, but non-pronomial and
  // non-list.
  private def filterRepresentativeMentions(graph: Map[Integer, CorefChain]): Map[Integer, CorefChain] =
    graph.filter({case (id, chain) =>
      val rm = chain.getRepresentativeMention
      this.isAnimate(rm) && !this.isPronomial(rm) && !this.isList(rm)
    })

  // Filters the chains in the direct coref. output of mentions that are animate, but non-pronomial and non-list.
  private def filterChains(graph: Map[Integer, CorefChain]): Map[Integer, CorefChain] = graph.map({case (id, chain) =>
    id -> new CorefChain(chain.getChainID,
      chain.getMentionMap.filter({case (ip, mentionSet) =>
        this.isAnimate(mentionSet.head) && !this.isPronomial(mentionSet.head) && !this.isList(mentionSet.head)}),
      chain.getRepresentativeMention)
  }).filter({case (id, chain) => !chain.getMentionMap.isEmpty})

  // Retrieves the gender for the given coref. mention. If coref. mention is titled with a gender specific title, then
  // the appropriate gender is returned, otherwise the gender assigned by the coreference resolution system is returned.
  private def getGender(m: CorefMention): Gender = {
    val hasMaleTitle = NameTitles.hasMaleTitle(m.mentionSpan)
    val hasFemaleTitle = NameTitles.hasFemaleTitle(m.mentionSpan)

    if (hasFemaleTitle && !hasMaleTitle)
      FEMALE
    else if (hasMaleTitle && !hasFemaleTitle)
      MALE
    else m.gender match {
      case Dictionaries.Gender.FEMALE => FEMALE
      case Dictionaries.Gender.MALE => MALE
      case Dictionaries.Gender.NEUTRAL | Dictionaries.Gender.UNKNOWN => UNKNOWN
    }
  }

  // Converts a Stanford CoreNLP's coreference mentions into the system representation.
  private def convertCorefMention(m: CorefMention): AliasCorefMention =
    AliasCorefMention(m.mentionSpan, this.getGender(m))

  /**
   * Runs coreference resolution on the given text, outputting a list of chains, each represented as a list of mentions.
   * @param text Text.
   * @return List of chains as lists of mentions.
   */
  def resolute(text: String): List[List[AliasCorefMention]] = {
    val document = new Annotation(text)
    this.pipeline.annotate(document)

    val graph: Map[Integer, CorefChain] = document.get(classOf[CorefChainAnnotation]).toMap
    this.filterChains(this.filterRepresentativeMentions(graph)).map({case (id, chain) =>
      chain
        .getMentionsInTextualOrder
        .map(convertCorefMention)
        .sortBy(_.span)
        .toList
    }).toList.sortBy(_.head.span)
  }

  /**
   * Runs coreference resolution on each text in a list, combining the resulting chains and returning a list of chains,
   * each represented as a list of mentions.
   * @param texts Sequence of texts.
   * @return List of chains as lists of mentions.
   */
  def resolute(texts: Seq[String]): List[List[AliasCorefMention]] =
    texts.flatMap(this.resolute).sortBy(_.head.span).toList

  /**
   * Runs coreference resolution on each of the paragraphes for the given story, outputting the combined list of chains,
   * with each chain represented as a list of mentions.
   * @param storyId Story id of story.
   * @return List of chains as lists of mentions.
   */
  override def resolute(storyId: StoryId): List[List[AliasCorefMention]] = {
    val corefCachedFile = new File(this.getCachedFilepath(storyId))

    if (corefCachedFile.exists())
      this.fromCacheCSV(corefCachedFile.getPath).asInstanceOf[List[List[AliasCorefMention]]]
    else storyId match {
      case StoryId(MOONSTONE, None) => throw new NotImplementedError
      case StoryId(PRIDE_AND_PREJUDICE, None) =>
        val coref = RawCorefResoluter.resolute(PrideAndPrejudice.getParagraphs)
        this.toCacheCSV(coref, corefCachedFile.getPath)
        coref
      case StoryId(SHERLOCK_HOLMES, Some(_)) =>
        val coref = RawCorefResoluter.resolute(SherlockHolmes.getParagraphs(storyId))
        this.toCacheCSV(coref, corefCachedFile.getPath)
        coref
      case StoryId(SILVER_STANDARD, Some(_)) =>
        val coref = RawCorefResoluter.resolute(SilverStandard.getParagraphs(storyId))
        this.toCacheCSV(coref, corefCachedFile.getPath)
        coref
      case _ => throw new StoryNotFoundException(storyId)
    }
  }

//  /**
//   * Runs coreference resolution on the text for the given story, outputting a list of chains, each represented as a
//   * list of mention strings.
//   * @param storyId Story id of story.
//   * @return List of chains as strings of mention spans.
//   */
//  def resolute(storyId: StoryId): List[List[String]] = {
//    val corefCachedFile = new File(this.getCachedFilepath(storyId))
//
//    if (corefCachedFile.exists())
////    {
////      var coref = this.fromCacheCSV(corefCachedFile.getPath)
////      coref = coref.map(_.map(CorefMentionNormalizer.norm).filterNot(_.isEmpty)).filterNot(_.isEmpty)
////      coref = coref.map(_.sorted).sortBy(_.head)
////      this.toCSV(coref, corefCachedFile.getPath)
////      coref
////    }
//      this.fromCacheCSV(corefCachedFile.getPath).asInstanceOf[List[List[String]]]
//    else storyId match {
//      // Apply resolution to each chapter of PP and then combine.
//      case StoryId(PRIDE_AND_PREJUDICE, None) =>
//        // Directory for storying coref. output per chapter.
//        val corefChaptersDir = new File(StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + "coref" +
//          File.separator + "chapters")
//
//        // Create the directory if it doesn't already exist.
//        if (!corefChaptersDir.exists())
//          corefChaptersDir.mkdirs()
//
//        var i = 1
//        val coref = PrideAndPrejudice.splitChapters.map(chapterText => {
//          val chapterCorefFile = new File(corefChaptersDir.getPath + File.separator + i + ".csv")
//
//          // Check if the coref. output file for the given chapter already exists.
//          if (chapterCorefFile.exists()) {
//            // Grab the coref. output from the file.
//            val chapterCoref = this.fromCacheCSV(chapterCorefFile.getPath).asInstanceOf[List[List[String]]]
//            i += 1
//            chapterCoref
//          } else {
//            // Otherwise generate it from the chapter text.
//            val chapterCoref = CorefResoluter.resolute(chapterText)
//            // Save the coref. output for each chapter
//            this.toCacheCSV(chapterCoref, chapterCorefFile.getPath)
//            i += 1
//            chapterCoref
//          }
//        }).flatten.sortBy(_.head).toList
//
//        // Save accumulated coref. output.
//        this.toCacheCSV(coref, corefCachedFile.getPath)
//        coref
//      case _ =>
//        val coref = CorefResoluter.resolute(TextManager.getText(storyId, ORIG))
//        this.toCacheCSV(coref, corefCachedFile.getPath)
//        coref
//    }
//  }

}

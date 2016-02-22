package network.alias.detection.detectors

import java.io.File
import java.util.Properties

import edu.stanford.nlp.dcoref.CorefChain.CorefMention
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.dcoref.{CorefChain, Dictionaries}
import edu.stanford.nlp.ling.CoreAnnotations.{TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap
import network.alias.Alias
import story.{StoryNotFoundException, PrideAndPrejudice, StoryId}
import story.StorySet._

import scala.collection.JavaConversions._


/**
 * Aliases extracted by Stanford's Coreference resolution system.
 */
object RawCorefDetector extends AliasDetector {

  /** Coref. cached aliases filename (for single stories) stored in standard .csv format. */
  override protected val cacheAliasesFilename: String = "coref.csv"
  /** Coref. cached aliases directory name for story collections with aliases stored in standard .csv format. */
  override protected val cacheAliasesDirname: String = "coref"

  // Properties for Stanford CoreNLP pipeline.
  private val props: Properties = new Properties()
  props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")

  // Stanford CoreNLP pipeline.
  private lazy val pipeline: StanfordCoreNLP = new StanfordCoreNLP(this.props)

  // Checks whether the given coref. mention is animate.
  private def isAnimate(m: CorefMention): Boolean = m.animacy == Dictionaries.Animacy.ANIMATE

  // Checks whether the given coref. mention is pronomial.
  private def isPronomial(m: CorefMention): Boolean = m.mentionType == Dictionaries.MentionType.PRONOMINAL

  // Checks whether the given coref. mention is list (whatever that is).
  private def isList(m: CorefMention): Boolean = m.mentionType == Dictionaries.MentionType.LIST

  // Filters the direct coref. output of chains with representative mentions that are animate, but non-list.
  private def filterRepresentativeMentions(graph: Map[Integer, CorefChain]): Map[Integer, CorefChain] =
    graph.filter({case (id, chain) =>
      val rm = chain.getRepresentativeMention
      this.isAnimate(rm) && !this.isList(rm)
    })

  // Filters the chains in the direct coref. output of mentions that are animate, but non-list.
  private def filterChains(graph: Map[Integer, CorefChain]): Map[Integer, CorefChain] = graph.map({case (id, chain) =>
    id -> new CorefChain(chain.getChainID,
      chain.getMentionMap.filter({case (ip, mentionSet) =>
        this.isAnimate(mentionSet.head) && !this.isList(mentionSet.head)}),
      chain.getRepresentativeMention)
  }).filter({case (id, chain) => !chain.getMentionMap.isEmpty})

  /**
   * Runs coreference resolution on the given text, outputting the mentions as a list of aliases.
   * @param text Text.
   * @return List of mention aliases.
   */
  def detect(text: String): List[Alias] = {
    val document = new Annotation(text)
    this.pipeline.annotate(document)

    val sentences: Array[CoreMap] = document.get(classOf[SentencesAnnotation]).toList.toArray
    val graph: Map[Integer, CorefChain] = document.get(classOf[CorefChainAnnotation]).toMap
    this.filterChains(this.filterRepresentativeMentions(graph)).flatMap({case (id, chain) =>
      chain
        .getMentionsInTextualOrder
        .map(m => {
          val tokens: Array[CoreLabel] = sentences(m.sentNum - 1).get(classOf[TokensAnnotation]).toList.toArray
          val startOffset = tokens(m.startIndex - 1).beginPosition()
          val endOffset = tokens(m.endIndex - 2).endPosition()
          val a = Alias(text.substring(startOffset, endOffset), startOffset, endOffset, None)
          println(a)
          a
        })
    }).toList.sorted
  }

  /**
   * Runs coreference resolution on each paragraph in the given sequence, combining the resulting mentions as a list of
   * aliases. (Assumes paragraphes are originally separated in text by double-CR.)
   * @param paragraphes Sequence of paragraphes.
   * @return List of mention aliases
   */
  def detect(paragraphes: Seq[String]): List[Alias] =
    paragraphes.foldLeft((List[Alias](), ""))({ case ((aliases, prevText), paragraphText) =>
      val paragraphAliases = this.detect(paragraphText)
      val offset = if (prevText.isEmpty) 0 else (prevText + "\n\n").length
      (aliases ++ paragraphAliases.map(a => Alias(a.span, a.startOffset + offset, a.endOffset + offset, a.aliasType)),
        if (prevText.isEmpty) paragraphText else prevText + "\n\n" + paragraphText)
    })._1

  /**
   * Runs coreference resolution on each of the paragraphes for the given story, outputting the combined mentions as a
   * list of aliases. (Assumes paragraphes are originally separated in text by double-CR.)
   * @param storyId Story id of story.
   * @return List of mention aliases.
   */
  override def detect(storyId: StoryId): List[Alias] = {
    val cacheFile = new File(this.getCachedFilepath(storyId))

    if (cacheFile.exists())
      this.fromCacheCSV(cacheFile.getPath).asInstanceOf[List[Alias]]
    else storyId match {
      case StoryId(PRIDE_AND_PREJUDICE, None) =>
        val aliases = this.detect(PrideAndPrejudice.getParagraphs).map(a =>
          // Random correction that's required.
          if (a.startOffset > 121900) Alias(a.span, a.startOffset - 1, a.endOffset - 1, a.aliasType) else a)
        this.toCacheCSV(aliases, cacheFile.getPath)
        aliases
      case StoryId(MOONSTONE, None) |
           StoryId(SHERLOCK_HOLMES, Some(_)) |
           StoryId(SILVER_STANDARD, Some(_)) |
           StoryId(ELSON, Some(_)) |
           StoryId(PROJECT_GUTENBERG, Some(_)) |
           StoryId(PIPER, Some(_)) |
           StoryId(WILKENS, Some(_)) => throw new NotImplementedError
      case _ => throw new StoryNotFoundException(storyId)
    }
  }

}

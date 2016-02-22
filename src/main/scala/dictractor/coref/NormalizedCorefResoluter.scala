package dictractor.coref

import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, PartOfSpeechAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.CoreLabel

import scala.collection.JavaConversions._

import java.io.File
import java.util.Properties

import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import lang.Adjectives
import story.StoryId


/**
 * Wrapper for Stanford's CoreNLP coreference resolution system that additionally normalizes the mention spans.
 */
object NormalizedCorefResoluter extends CorefResoluter {

  /** Resoluter id. */
  override val id: String = "norm"

  /** Cache filename for the normalized coref. output of a single story. */
  override protected val cacheStoryFilename: String = this.id + ".csv"
  /** Cache directory name for the normalized coref. output of the texts in a story collection. */
  override protected val cacheStoryDirname: String = this.id

  // Stanford CoreNLP pipeline for POS tagging.
  private lazy val props = new Properties()
  props.setProperty("annotators", "tokenize, ssplit, pos")
  private lazy val pipeline: StanfordCoreNLP = new StanfordCoreNLP(this.props)

  /**
   * Normalizes the mention in the given list of coref. chains, with each chain represented as a list of mentions.
   * @param coref Coref. chains with each chain represented as a list of mentions.
   * @return Normalized coref. chains.
   */
  def resolute(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    coref.map(_
        .map(m => AliasCorefMention(this.normalizeSpan(m.span), m.gender))
        .filterNot(_.span.isEmpty)
        .sortBy(_.span))
      .filterNot(_.isEmpty)
      .sortBy(_.head.span)

  /**
   * Normalizes the mentions in the [[dictractor.coref.RawCorefResoluter]] output for the given story.
   * @param storyId Story id of story.
   * @return Normalized coref. chains.
   */
  def resolute(storyId: StoryId): List[List[AliasCorefMention]] = {
    val cachedFile = new File(this.getCachedFilepath(storyId))

    if (cachedFile.exists())
      this.fromCacheCSV(cachedFile.getPath).asInstanceOf[List[List[AliasCorefMention]]]
    else {
      val coref = this.resolute(RawCorefResoluter.resolute(storyId))
      this.toCacheCSV(coref, cachedFile.getPath)
      coref
    }
  }

  /**
   * Normalizes the given span to the head noun phrase portion.
   * @param span Span.
   * @return Normalized span.
   */
  def normalizeSpan(span: String): String = {
    val isUppercase: Boolean = span.toUpperCase == span

    val annotation: Annotation = new Annotation(span)
    this.pipeline.annotate(annotation)

    val tokens: Array[CoreLabel] =
      annotation.get(classOf[SentencesAnnotation]).flatMap(_.get(classOf[TokensAnnotation])).toArray

    def isNN(token: CoreLabel): Boolean = {
      val tag: String = token.get(classOf[PartOfSpeechAnnotation])
      tag.startsWith("NN")
    }

    var normalizedSpanTokens: List[(Int, String)] = List()
    var inFirstNouns = false
    var passedFirstNouns: Boolean = false
    for (i <- tokens.indices) {
      val isNoun = isNN(tokens(i))
      if (isNoun && !inFirstNouns && !passedFirstNouns) {
        normalizedSpanTokens :+= (i, tokens(i).get(classOf[TextAnnotation]))
        inFirstNouns = true
      } else if (isNoun && inFirstNouns && !passedFirstNouns) {
        normalizedSpanTokens :+= (i, tokens(i).get(classOf[TextAnnotation]))
      } else if (!isNoun && inFirstNouns && !passedFirstNouns) {
        inFirstNouns = false
        passedFirstNouns = true
      }
    }

    if (normalizedSpanTokens.nonEmpty && normalizedSpanTokens.head._2(0).isUpper) {
      val endIndex = normalizedSpanTokens.last._1
      if (!isUppercase && endIndex < tokens.length - 2) {
        val nextTokenString = tokens(endIndex + 1).get(classOf[TextAnnotation])
        val nextTokenStringLower = nextTokenString.toLowerCase
        if (nextTokenStringLower == "of" | nextTokenStringLower == "the") {
          var i: Int = endIndex + 2
          var ts: String = tokens(i).get(classOf[TextAnnotation])

          if (ts(0).isUpper) {
            normalizedSpanTokens :+= (endIndex + 1, nextTokenString)

            while (i < tokens.length && ts(0).isUpper) {
              normalizedSpanTokens :+= (i, ts)
              i += 1
              if (i < tokens.length)
                ts = tokens(i).get(classOf[TextAnnotation])
            }
          }
        }
      }
    }

    // Check that the given span contains a capitalized token if the input span is not all uppercase, in which case
    // drop any leading tokens that are lowercase.
    if (!isUppercase && normalizedSpanTokens.exists({ case (i, t) => t(0).isUpper })) {
        while (normalizedSpanTokens.head._2(0).isLower)
          normalizedSpanTokens = normalizedSpanTokens.drop(1)
    }

    // Drop any starting 'Dear', 'Dearest', 'Lets', or adjective.
    if (normalizedSpanTokens.nonEmpty &&
      (normalizedSpanTokens.head._2 == "Dear" ||
        normalizedSpanTokens.head._2 == "Dearest" ||
        normalizedSpanTokens.head._2 == "Lets" ||
        Adjectives.isAdjective(normalizedSpanTokens.head._2)))
      normalizedSpanTokens = normalizedSpanTokens.drop(1)

    normalizedSpanTokens.map(_._2).mkString(" ")
  }

}

package network.alias.detection.detectors

import java.io.File
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.{PartOfSpeechAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import network.alias.Alias
import story.StoryId
import text.{STRIPPED, TextManager}

import scala.collection.JavaConversions._


/**
 * Normalizes the output of [[network.alias.detection.detectors.RawCorefDetector]].
 */
object NormalizingCorefDetector extends AliasDetector {

  /** Normalized coref. cached aliases filename (for single stories) stored in standard .csv format. */
  override protected val cacheAliasesFilename: String = "coref-norm.csv"
  /** Normalized coref. cached aliases directory name for story collections with aliases stored in standard .csv
    * format. */
  override protected val cacheAliasesDirname: String = "coref-norm"

  // Stanford CoreNLP pipeline for POS tagging.
  private lazy val props = new Properties()
  props.setProperty("annotators", "tokenize, ssplit, pos")
  private lazy val pipeline: StanfordCoreNLP = new StanfordCoreNLP(this.props)

  /**
   * Normalizes the span for the given alias to the head noun phrase portion. If the head is a non-possessive pronoun,
   * then only the head is returned. If the head is neither, than None is returned.
   * @param alias Alias.
   * @param text Text from which aliases were derived.
   * @return Option of an alias.
   */
  def normalizeSpan(alias: Alias, text: String): Option[Alias] = {
    def isNN(token: CoreLabel): Boolean = token.get(classOf[PartOfSpeechAnnotation]).startsWith("NN")

    def isNonPossessivePronoun(token: CoreLabel): Boolean = token.get(classOf[PartOfSpeechAnnotation]) == "PRP"

    val span = alias.span
    val isUppercase: Boolean = span.toUpperCase == span

    val annotation: Annotation = new Annotation(span)
    this.pipeline.annotate(annotation)

    val tokens: Array[CoreLabel] =
      annotation.get(classOf[SentencesAnnotation]).get(0).get(classOf[TokensAnnotation]).toList.toArray

    var normalizedTokens: List[(Int, CoreLabel)] = List()
    var inFirstNouns = false
    var passedFirstNouns: Boolean = false
    for (i <- tokens.indices) {
      // 'de' is a Noun when it appears in names (e.g. 'Sir Lewis de Bourgh') but is mis-tagged by POS tagger.
      val isNoun = isNN(tokens(i)) || tokens(i).originalText() == "de"
      if (isNoun && !inFirstNouns && !passedFirstNouns) {
        normalizedTokens :+=(i, tokens(i))
        inFirstNouns = true
      } else if (isNoun && inFirstNouns && !passedFirstNouns) {
        normalizedTokens :+=(i, tokens(i))
      } else if (!isNoun && inFirstNouns && !passedFirstNouns) {
        inFirstNouns = false
        passedFirstNouns = true
      }
    }

    if (normalizedTokens.isEmpty) {
      val nonPossessivePronouns = tokens.indices.filter(i => isNonPossessivePronoun(tokens(i))).map(i => (i, tokens(i)))
      if (nonPossessivePronouns.nonEmpty)
        normalizedTokens :+= nonPossessivePronouns.head
    }

    // For handling regnal-type 'of's and 'the's.
    //    if (normalizedSpanTokens.nonEmpty && normalizedSpanTokens.head._2(0).isUpper) {
    //      val endIndex = normalizedSpanTokens.last._1
    //      if (!isUppercase && endIndex < tokens.length - 2) {
    //        val nextTokenString = tokens(endIndex + 1).get(classOf[TextAnnotation])
    //        val nextTokenStringLower = nextTokenString.toLowerCase
    //        if (nextTokenStringLower == "of" | nextTokenStringLower == "the") {
    //          var i: Int = endIndex + 2
    //          var ts: String = tokens(i).get(classOf[TextAnnotation])
    //
    //          if (ts(0).isUpper) {
    //            normalizedSpanTokens :+= (endIndex + 1, nextTokenString)
    //
    //            while (i < tokens.length && ts(0).isUpper) {
    //              normalizedSpanTokens :+= (i, ts)
    //              i += 1
    //              if (i < tokens.length)
    //                ts = tokens(i).get(classOf[TextAnnotation])
    //            }
    //          }
    //        }
    //      }
    //    }
    //

    // Check that the given span contains a capitalized token if the input span is not all uppercase, in which case
    // drop any leading tokens that are lowercase.
    if (!isUppercase && normalizedTokens.exists({ case (i, t) => t.originalText()(0).isUpper })) {
      while (normalizedTokens.head._2.originalText()(0).isLower)
        normalizedTokens = normalizedTokens.drop(1)
    }

    // Drop any starting 'Dearest' or 'Lets'.
    if (normalizedTokens.nonEmpty &&
        (normalizedTokens.head._2.originalText() == "Dearest" ||
          normalizedTokens.head._2.originalText() == "Lets"))
      normalizedTokens = normalizedTokens.drop(1)

    if (normalizedTokens.nonEmpty) {
      val startOffset = normalizedTokens.head._2.beginPosition() + alias.startOffset
      val endOffset = normalizedTokens.last._2.endPosition() + alias.startOffset
      Some(Alias(text.substring(startOffset, endOffset), startOffset, endOffset, alias.aliasType))
    } else
      None
  }

  /**
   * Extracts the list of normalized coref. aliases for the given story, by applying
   * [[network.alias.detection.detectors.NormalizingCorefDetector.normalizeSpan]] on each alias outputted by
   * [[network.alias.detection.detectors.RawCorefDetector]].
   * @param storyId Story id of story.
   * @return List of extracted aliases.
   */
  override def detect(storyId: StoryId): List[Alias] = {
    val cacheFile = new File(this.getCachedFilepath(storyId))

    if (cacheFile.exists())
      this.fromCacheCSV(cacheFile.getPath).asInstanceOf[List[Alias]]
    else {
      val strippedText = TextManager.getText(storyId, STRIPPED)
      val aliases = RawCorefDetector.detect(storyId)
        .map(this.normalizeSpan(_, strippedText))
        .filterNot(_.isEmpty)
        .map(_.get)
      this.toCacheCSV(aliases, cacheFile.getPath)
      aliases
    }
  }

}

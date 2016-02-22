package lang

import java.io.File
import java.util.Properties

import data.Data
import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, PartOfSpeechAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{StanfordCoreNLP, Annotation}

import scala.collection.JavaConversions._


/**
 * Manages set of nouns.
 */
object AnimateNouns {

  /** Path to directory with animate nouns. */
  val DIRPATH = Data.DIRPATH + File.separator + "animate-nouns"

  /** Filename for file with WordNet animate nouns. */
  val WORDNET_FILENAME = "wordnet-normed.txt"

  /** Filename for file with Crown animate nouns. */
  val CROWN_FILENAME = "crown-normed.txt"

  // For Stanford's CoreNLP pipeline.
  private lazy val props = new Properties()
  props.setProperty("annotators", "tokenize, ssplit, pos")
  // Stanford's CoreNLP pipeline for POS tagging.
  private lazy val pipeline: StanfordCoreNLP = new StanfordCoreNLP(this.props)

  // Loads the animate nouns from a file located by the given filepath, where each noun is listed on its own line.
  private def load(filepath: String): Set[String] =
    io.Source.fromFile(new File(filepath)).getLines().map(_.trim).filterNot(_.isEmpty).toSet

  /** Set of all animate nouns. */
  lazy val animateNouns: Set[String] = this.load(this.DIRPATH + File.separator + this.WORDNET_FILENAME) |
    this.load(this.DIRPATH + File.separator + this.CROWN_FILENAME)

  // Normalizes a span in the original WordNet and Crown lists.
  private def normalizeSpan(span: String): String = {
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
        normalizedSpanTokens :+=(i, tokens(i).get(classOf[TextAnnotation]))
        inFirstNouns = true
      } else if (isNoun && inFirstNouns && !passedFirstNouns) {
        normalizedSpanTokens :+=(i, tokens(i).get(classOf[TextAnnotation]))
      } else if (!isNoun && inFirstNouns && !passedFirstNouns) {
        inFirstNouns = false
        passedFirstNouns = true
      }
    }

    normalizedSpanTokens.map(_._2).mkString(" ")
  }

}

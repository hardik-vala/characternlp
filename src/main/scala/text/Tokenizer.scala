package text

import collection.JavaConversions._

import java.io.{File, StringReader}

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{DocumentPreprocessor, CoreLabelTokenFactory, PTBTokenizer}

/**
 * Wrapper for CoreNLP tokenizer.
 */
object Tokenizer {

  /**
   * Tokenizes the text in given text file and returns it as a string with each sentence separated by a CR.
   * @param textFile Text file.
   * @return Tokenized text.
   */
  def tokenize(textFile: File): String =
    (new DocumentPreprocessor(textFile.getPath))
      .map(_.map(_.word).mkString(" "))
      .mkString("\n")

  /**
   * Tokenizes the given text and returns it as a string.
   * @param text Text to tokenize.
   * @return Tokenized text.
   */
  def tokenize(text: String): String = {
    val ptbt: PTBTokenizer[CoreLabel] = new PTBTokenizer[CoreLabel](new StringReader(text),
      new CoreLabelTokenFactory(), "")

    ptbt.tokenize().map(_.word).mkString(" ")
  }

}

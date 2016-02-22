package booknlp

import java.io.File


/**
 * Runner for BookNLP.
 */
object BookNLP {

  /**
   * Run BookNLP with the text specified by the input filepath and output saved to the given output directory path.
   * @param inputPath Input filepath to text.
   * @param outputDirPath Output directory path.
   */
  def run(inputPath: String, outputDirPath: String): Unit =
    novels.BookNLP.main(Array("-doc", inputPath, "-printHTML", "-p", outputDirPath, "-tok",
      outputDirPath + File.separator + "tokens", "-f"))

}

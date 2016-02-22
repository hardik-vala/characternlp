package text


/**
 * Splits texts.
 */
object TextSplitter {

  /** Single CR. */
  val SINGLE_CR = "\r"
  /** Single LF. */
  val SINGLE_LF = "\n"
  /** Double LF. */
  val DOUBLE_LF = "\n\n"
  /** Double CR-LF. */
  val DOUBLE_CRLF = "\r\n\r\n"

  /**
   * Splits a text into paragraphs.
   * @param text Text to split.
   * @param separator Paragraph separator (Default is double CR).
   * @return Array of paragraphs.
   */
  def splitParagraphs(text: String, separator: String = this.DOUBLE_LF): Array[String] =
    text.split(separator).filterNot(_.isEmpty).map(_.trim)

}

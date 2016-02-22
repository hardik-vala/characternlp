package text

/**
 * Text type.
 */
trait TextType
/** Original text. */
case object ORIG extends TextType
/** Original text with all extraneous text removed (including chapter designations). */
case object STRIPPED extends TextType
/** Tokenized text. */
case object TOK extends TextType
/** NER annotated text. */
case object NER extends TextType
/** NER (with titles) annotated text */
case object NER_WT_TITLES extends TextType

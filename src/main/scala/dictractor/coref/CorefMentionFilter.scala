package dictractor.coref

import lang.{Determiners, Pronouns}


/**
 * Filter on coreference resolution mentions.
 */
trait CorefMentionFilter {

  /** Filter id. */
  val id: String

  /**
   * Filters the coreference mentions according to some predicate.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]]

}

/**
 * Filters out mentions that have t or more lowercase tokens.
 * @param t Threshold.
 */
class LowerCaseTokenFilter(t: Int) extends CorefMentionFilter {

  /** Filter id. */
  override val id: String = "lt" + t

  /**
   * Filters out coreference mentions that have t or more lowercase tokens.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    coref.map(_.filterNot(_.span.split("\\s+").count(k => k == k.toLowerCase) >= this.t)).filterNot(_.isEmpty)

}

/**
 * Filters out all uppercase mentions.
 */
object AllUppercaseFilter extends CorefMentionFilter {

  /** Filter id. */
  override val id: String = "uc"

  /**
   * Filters out the all uppercase mentions.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    coref.map(_.filterNot(a => a.span == a.span.toUpperCase)).filterNot(_.isEmpty)
}

/**
 * Filters out mentions that are completely lowercase.
 */
object LowerCaseFilter extends CorefMentionFilter {

  /** Filter id. */
  override val id: String = "lc"

  /**
   * Filters out mentions that are completely lowercase.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    coref.map(_.filterNot(a => {a.span.toLowerCase == a.span})).filterNot(_.isEmpty)

}

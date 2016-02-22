package dictractor.coref

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import lang.{Pronouns, Determiners}
import name.{NameGazetteer, NameTitles}


/**
 * Filter on coreference resolution chains.
 */
trait CorefChainFilter {

  /** Filter id. */
  val id: String

  /**
   * Filters the list of coreference chains according to some predicate.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]]

}

/**
 * Filters out coreference chains that contain mentions with both male and female genders.
 */
object GenderCorefChainFilter extends CorefChainFilter {

  /** Filter id. */
  val id: String = "gen"

  /**
   * Filters the list of coreference chains of chains containing at least one mention with male gender and at least one
   * mention with female gender.
   * @param coref List of coreference chains, each represented as a list of mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] = coref.filterNot(c => {
    c.exists(_.isFemale) && c.exists(_.isMale)
  })

}

/**
 * Filters out coreference chains containing solely non-name mentions.
 */
object NonNameCorefChainFilter extends CorefChainFilter {

  /** Filter id. */
  val id: String = "nn"

  /**
   * Filters the list of coreference chains of chains consisting only of non-name mentions.
   * @param coref List of coreference chains, each represented as a list of string mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    coref.filterNot(_.map(m => {
      val tokens = m.span.split(" ")
      if (Determiners.startsWithDeterminer(m.span) || Pronouns.startsWithPronoun(m.span))
        AliasCorefMention(tokens.slice(1, tokens.length).mkString(" "), m.gender)
      else m
    }).forall(m => m.span == m.span.toLowerCase))

}

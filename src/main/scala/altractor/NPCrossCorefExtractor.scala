package altractor

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import dict.{GenderedAlias, Alias}
import dictractor.coref._
import name._
import story.StoryId


/**
 * Extracted aliases by cross-referenceing Stanford NER's named person output (with titles included as well as any
 * additional titled names) with the coreference chains output by the given coreference resoluter.
 * @param corefResoluter Coreference resoluer.
 */
class NPCrossCorefExtractor(corefResoluter: CorefResoluter) extends AliasExtractor {

  /** Filename for cached extracted aliases (Applies to single stories). */
  override protected val cacheAliasesFilename: String = File.separator + "npt-coref.csv"
  /** Directory name for cached extracted aliases (Applies to story collections). */
  override protected val cacheAliasesDirname: String = File.separator + "npt-coref"

  /**
   * Loads extracted gendered aliases as a set from a cached file.
   * @param filepath Filepath location of cached list of gendered aliases.
   * @return Set of gendered aliases.
   */
  override protected def fromCacheCSV(filepath: String): Any =
    io.Source.fromFile(new File(filepath)).getLines().map(s => {
      val ss: Array[String] = s.split(" \\[")
      val gender = ss(1).substring(0, ss(1).length - 1) match {
        // TODO: Figure out better way to handle dangling square bracket.
        case "FEMALE" | "FEMALE]" => FEMALE
        case "MALE" | "MALE]" => MALE
        case "UNKNOWN" | "UNKNOWN]" => UNKNOWN
      }
      new GenderedAlias(ss(0), gender)
    }).toSet

  /**
   * Saves the given extracted gendered aliases as a cached file specified by the given filepath.
   * @param aliases Extracted gendered aliases to save.
   * @param filepath Filepath location to save to.
   */
  override protected def toCacheCSV(aliases: Any, filepath: String): Unit =
    CSVWriter.open(new File(filepath)).writeAll(aliases.asInstanceOf[Set[GenderedAlias]].map(ga =>
      ga.span + " [" + ga.gender + "]").toList.map(_::Nil))

  /**
   * Extracts the set of gendered aliases by cross-referencing the set of named persons with the given coref. output.
   * @param nps Set of named person aliases.
   * @param coref Coref. chains as a list of chains comprised of mentions.
   * @return Extracted set of gendered aliases.
   */
  def extract(nps: Set[_ <: Alias], coref: List[List[AliasCorefMention]]): Set[GenderedAlias] = {
    val chainSets: List[Set[AliasCorefMention]] = coref.map(_.toSet)
    val crossCoref: Set[GenderedAlias] = nps
      .flatMap(np => chainSets.filter(_.exists(_.span == np.span)).flatten)
      .map(m => new GenderedAlias(m.span, m.gender))
    val nonCorefNPs: Set[GenderedAlias] = nps
      .filterNot(np => crossCoref.exists(_.span == np.span))
      .map(np => new GenderedAlias(np.span, Gender.assign(np.span)))

    (crossCoref | nonCorefNPs)
      // Filter out all upper case aliases.
      .filterNot(a => a.span.toUpperCase == a.span)
      // Drop leading capitalized modifiers.
      .map(a => {
        val lowerSpan = a.span.toLowerCase
        if (lowerSpan.startsWith("dearest "))
          new GenderedAlias(a.span.substring("dearest ".length, a.span.length).trim(), a.gender)
        else if (lowerSpan.startsWith("dear "))
          new GenderedAlias(a.span.substring("dear ".length, a.span.length).trim(), a.gender)
        else if (lowerSpan.startsWith("lets "))
          new GenderedAlias(a.span.substring("lets ".length, a.span.length).trim(), a.gender)
        else if (lowerSpan.startsWith("let "))
          new GenderedAlias(a.span.substring("let ".length, a.span.length).trim(), a.gender)
        else
          a
        })
      // Correct any genders for proper names.
      .map(a => {
        val inferredGender = Gender.assign(a.span)
        if (inferredGender == UNKNOWN) a else new GenderedAlias(a.span, inferredGender)
    })
  }

  /**
   * Extracts the set of extracted aliases in a given story.
   * @param storyId Story id of story.
   * @return Set of named persons with titles.
   */
  override def extract(storyId: StoryId): Set[_ <: Alias] = {
    if (this.inCache(storyId))
      this.fromCache(storyId).asInstanceOf[Set[Alias]]
    else {
      val aliases = this.extract(NPPlusTitledExtractor.extract(storyId), corefResoluter.resolute(storyId))
        // Ignore special narrator aliases.
        .filterNot(this.isNarrator)
      this.toCache(storyId, aliases)
      aliases
    }
  }

}

object NPCrossCorefExtractorRunner extends App {
  val aliasExtractor = new NPCrossCorefExtractor(new FilteredMentionCorefResoluter(NormalizedCorefResoluter,
    AllUppercaseFilter::LowerCaseFilter::Nil))
//  aliasExtractor.extract(new StoryId(story.StorySet.MOONSTONE))
  story.ProjectGutenberg.ids.foreach(id => {
    println("Starting " + id.storyName.get)
    aliasExtractor.extract(id)
    println("Completed " + id.storyName.get)
  })
//  story.Elson.ids.foreach(id => {
//    println("Starting " + id.storyName.get)
//    aliasExtractor.extract(id)
//    println("Completed " + id.storyName.get)
//  })

}

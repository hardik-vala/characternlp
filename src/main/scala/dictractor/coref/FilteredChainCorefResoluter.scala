package dictractor.coref

import java.io.File

import story.{StoryNotFoundException, StoryId}
import story.StorySet._


/**
 * Wrapper for the given coreference resoluter that additionally filters the outputted coref. chains.
 * @param corefResoluter Coreference resoluter to wrap.
 * @param filters Sequence of coref. chain filters to apply (in order).
 */
class FilteredChainCorefResoluter(corefResoluter: CorefResoluter,
                             filters: Seq[CorefChainFilter]) extends CorefChainFilter with CorefResoluter {

  /** Resoluter id (Comprised of the argument coref. resoluter and the filter id's concatenated with '-'). */
  override val id: String = "chainfilt-" + corefResoluter.id + "." + this.filters.map(_.id).mkString("+")

  /** Cache filename for the filtered coref. output of a single story. */
  val cacheStoryFilename = this.id + ".csv"
  /** Cache directory name for filtered coref. output of the texts in a story collection. */
  val cacheStoryDirname = this.id

  // Applies the filters in sequence to the given coref. chains.
  private def filt(coref: List[List[AliasCorefMention]],
                   filters: Seq[CorefChainFilter]): List[List[AliasCorefMention]] =
    filters.foldLeft (coref) {case (c: List[List[AliasCorefMention]], f: CorefChainFilter) => f.filt(c)}

  /**
   * Filters the list of coreference chains according to the sequence of filters.
   * @param coref List of coreference chains, each represented as a list of string mentions.
   * @return Filtered list of coreference chains.
   */
  override def filt(coref: List[List[AliasCorefMention]]): List[List[AliasCorefMention]] =
    this.filt(coref, this.filters)

  /**
   * Generates the filtered list of coreference chains for the given story according to the sequence of filters and
   * starting from the output of the given coreference resoluter.
   * @param storyId Story id of story.
   * @return Filtered list of coreference chains.
   */
  override def resolute(storyId: StoryId): List[List[AliasCorefMention]] = {
    val cachedFile = new File(this.getCachedFilepath(storyId))

    if (cachedFile.exists())
      this.fromCacheCSV(cachedFile.getPath).asInstanceOf[List[List[AliasCorefMention]]]
    else storyId match {
      case StoryId(MOONSTONE, None) => throw new NotImplementedError
      case StoryId(PRIDE_AND_PREJUDICE, None) | StoryId(SHERLOCK_HOLMES, Some(_)) | StoryId(SILVER_STANDARD, Some(_)) =>
        val filteredCoref = this.filt(corefResoluter.resolute(storyId))
        this.toCacheCSV(filteredCoref, cachedFile.getPath)
        filteredCoref
      case _ => throw new StoryNotFoundException(storyId)
    }
  }

}

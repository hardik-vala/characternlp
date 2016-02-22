package dictractor.extractors.proposed

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import dictractor.coref._
import story._
import story.StorySet._
import utils.Cache


/**
 * Generates pairs of mentions according to co-occurrence in coreference chains, according to the given coreference
 * resoluter.
 * @param corefResoluter Coreference resoluter.
 */
class CorefPairGenerator(corefResoluter: CorefResoluter) extends Cache {
  // Cache filename for the coref. output of a single story.
  private val cacheFilename = corefResoluter.id + "-pairs.csv"
  // Cache directory name for the coref. output of a story collection.
  private val cacheDirname = corefResoluter.id + "-pairs"

  /**
   * Saves the coref. pairs (with co-occurrence counts) to a .csv file located by the given path
   * @param o Coref. pairs to cache.
   * @param filepath Filepath location of cache .csv file.
   */
  override def toCacheCSV(o: Any, filepath: String): Unit = {
    val pairs = o.asInstanceOf[Map[(String, String), Int]]
    val pairsInOrder: List[((String, String), Int)] = pairs.toList.sortBy({ case (p, n) => -n })
    CSVWriter.open(new File(filepath)).writeAll(pairsInOrder.map({case ((p1, p2), n) => List(p1, p2, n.toString)}))
  }

  /**
   * Loads the coref. pairs from a .csv file located by the given filepath as a map between pairs and counts.
   * @param filepath Filepath location of cache .csv file.
   * @return Mapping from coref. pairs to co-occurrence counts.
   */
  override def fromCacheCSV(filepath: String): Any =
    CSVReader.open(new File(filepath)).all().map(row => (row.head, row(1)) -> row(2).toInt).toMap

  /**
   * Retrieves the filepath to the cached coref. pairs for the given story.
   * @param storyId Story id of story.
   * @return Filepath to cache .csv file of coref. pairs with counts.
   */
  override protected def getCachedFilepath(storyId: Any): String = storyId match {
    case StoryId(MOONSTONE, None) =>
      val parentDir = new File(StoryOverseer.getPath(MOONSTONE) + File.separator + "coref")

      // Creates the parent directory if it doesn't already exist.
      if (!parentDir.exists())
        parentDir.mkdirs()

      parentDir.getPath + File.separator + this.cacheFilename
    case StoryId(PRIDE_AND_PREJUDICE, None) =>
      val parentDir = new File(StoryOverseer.getPath(PRIDE_AND_PREJUDICE) + File.separator + "coref")

      // Creates the parent directory if it doesn't already exist.
      if (!parentDir.exists())
        parentDir.mkdirs()

      parentDir.getPath + File.separator + this.cacheFilename
    case StoryId(SHERLOCK_HOLMES, Some(name))=>
      val dir = new File(StoryOverseer.getPath(SHERLOCK_HOLMES) + File.separator + "coref" +
        File.separator + this.cacheDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(SILVER_STANDARD, Some(name))=>
      val dir = new File(StoryOverseer.getPath(SILVER_STANDARD) + File.separator + "coref" +
        File.separator + this.cacheDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(ELSON, Some(name))=>
      val dir = new File(StoryOverseer.getPath(ELSON) + File.separator + "coref" +
        File.separator + this.cacheDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case StoryId(PROJECT_GUTENBERG, Some(name))=>
      val dir = new File(StoryOverseer.getPath(PROJECT_GUTENBERG) + File.separator + "coref" +
        File.separator + this.cacheDirname)

      if (!dir.exists())
        dir.mkdirs()

      dir.getPath + File.separator + name + ".csv"
    case _: StoryId => throw new StoryNotFoundException(storyId.asInstanceOf[StoryId])
    case _ => throw new IllegalArgumentException("Expected story Id.")
  }

  /**
   * Clears the cache.
   */
  override protected def clearCache: Unit = {
    for (storySet <- StorySet.values) {
      storySet match {
        case MOONSTONE => this.deleteFromCache(new StoryId(MOONSTONE))
        case PRIDE_AND_PREJUDICE => this.deleteFromCache(new StoryId(PRIDE_AND_PREJUDICE))
        case SHERLOCK_HOLMES =>
          // Delete all cached dictionaries in cache directories.
          SherlockHolmes.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SherlockHolmes.ids.head)).getParent).delete()
        case SILVER_STANDARD =>
          // Delete all cached dictionaries in cache directories.
          SilverStandard.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(SilverStandard.ids.head)).getParent).delete()
        case ELSON =>
          // Delete all cached dictionaries in cache directories.
          Elson.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(Elson.ids.head)).getParent).delete()
        case PROJECT_GUTENBERG =>
          // Delete all cached dictionaries in cache directories.
          ProjectGutenberg.ids.foreach(this.deleteFromCache)
          // Delete cache directory.
          new File(new File(this.getCachedFilepath(ProjectGutenberg.ids.head)).getParent).delete()
        case _ => throw new RuntimeException("Unhandled story set.")
      }
    }
  }

  /**
   * Generates the coref. pairs from the given list of coref. chains, outputting a mapping from pairs to chain
   * co-occurrence counts.
   * @param coref List of coref. chains, with each chain represented as a mention list.
   * @return Mapping from (String) pairs to chain co-occurrence counts.
   */
  def gen(coref: List[List[AliasCorefMention]]): Map[(String, String), Int] = {
    var i = 0
    coref
      // Filter out singleton chains.
      .filter(_.size > 1)
      // Map each mention to its span.
      .map(_.map(_.span))
      // Convert each mention string into a unique string by tacking on a unique id.
      .map(_.map(s => { i += 1 ; s + "<->" + i }))
      // Generate combinations in each chaing of size  2.
      .map(_.combinations(2))
      // Strip the unique ids.
      .map(_.map(_.map(_.split("<->")(0))))
      // Filter out combinations involving identical mentions.
      .map(_.filterNot(p => p.head == p(1)).toList)
      // Filter out resulting empty chains.
      .filterNot(_.isEmpty)
      // Covert to combination pairs from list to tuple.
      .flatMap(_.map(p => (p.head, p(1))))
      // Group by tuple instances.
      .groupBy(p => p)
      // Get mapping from tuple to number of occurrences.
      .mapValues(_.size)
  }

  /**
   * Generates the coref. pairs for the given story, outputting a mapping from pairs to chain co-occurrence counts.
   * @param storyId Story id of story.
   * @return Mapping from (String) pairs to chain co-occurrence counts.
   */
  def gen(storyId: StoryId): Map[(String, String), Int] = {
    if (this.inCache(storyId))
      this.fromCache(storyId).asInstanceOf[Map[(String, String), Int]]
    else {
      val pairs = this.gen(corefResoluter.resolute(storyId))
      this.toCache(storyId, pairs)
      pairs
    }
  }

}

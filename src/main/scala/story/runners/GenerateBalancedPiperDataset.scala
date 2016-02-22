package story.runners

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import story.{StoryOverseer, PiperStory, StoryId, Piper}
import story.StorySet.PIPER


/**
 * Generates a balanced subset of the Piper dataset as a list of story id's by taking the w closest stories for each
 * decade to the average world catalogue index for that decade.
 */
object GenerateBalancedPiperDataset extends App {

  // W closest stories to keep for each decade according to the average world catalogue index for that decade.
  val W: Int = 20

  // Map from Piper story id's to the story objects.
  val piperStories: Map[StoryId, PiperStory] = Piper.storyMap
  // Grouping the Piper stories by decade.
  val piperStoriesByDecade: Map[Int, Iterable[PiperStory]] = piperStories.values.groupBy(_.year / 10)
  // Mapping from the decade to the average world catalogue index for that decade.
  val averageWorldCatalogueIndicesByDecade: Map[Int, Double] = piperStoriesByDecade.mapValues(storyList =>
    storyList.map(_.worldCatalogueIndex).sum.toDouble / storyList.size)
  // List of story id's for the balanced set.
  val balancedDatasetIds: Iterable[StoryId] =
    piperStoriesByDecade.map({ case (decade: Int, storyList: Iterable[PiperStory]) =>
      decade -> storyList.toList.sortBy(story =>
        Math.abs(averageWorldCatalogueIndicesByDecade.get(decade).get - story.worldCatalogueIndex)).take(W).map(_.id)
    }).values.flatten

  // Write the story id names to a file, with one name per line.
  CSVWriter
    .open(new File(StoryOverseer.getPath(PIPER) + File.separator + "balanced-" + W + ".txt"))
    .writeAll(balancedDatasetIds.map(id => List(id.storyName.get)).toList)

}

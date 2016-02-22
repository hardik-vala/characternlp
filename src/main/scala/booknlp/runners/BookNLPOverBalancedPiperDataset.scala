package booknlp.runners

import java.io.File

import booknlp.BookNLP
import story.StorySet.PIPER
import story.{Piper, StoryId, StoryOverseer}
import text.{ORIG, TextManager}


/**
 * Runs BookNLP over each of the texts in the balanced Piper dataset.
 */
object BookNLPOverBalancedPiperDataset extends App {

  // Filepath to the list of ids in the balanced Piper dataset.
  val idsFilepath = StoryOverseer.getPath(PIPER) + File.separator + "balanced-20.txt"
  // Set of id's.
  val ids: Set[String] = io.Source.fromFile(new File(idsFilepath)).getLines().toSet

  // Story id's.
  val storyIds: List[StoryId] = Piper.ids.filter(sid => ids.contains(sid.storyName.get))

  // Output BookNLP directory path.
  val outputBookNLPDirpath = StoryOverseer.getPath(PIPER) + File.separator + "book-nlp"
  val outputBookNLPDir = new File(outputBookNLPDirpath)
  // Create the directory if it doesn't already exist.
  if (!outputBookNLPDir.exists())
    outputBookNLPDir.mkdirs()

  storyIds.foreach(sid => {
    val storyOutputDir = new File(outputBookNLPDirpath + File.separator + sid.storyName.get)
    if (!storyOutputDir.exists() || (storyOutputDir.exists() && storyOutputDir.list().length <= 1)) {
      println("Starting " + sid.storyName.get)
      try {
        BookNLP.run(TextManager.getTextFile(sid, ORIG).getPath, storyOutputDir.getPath)
        println("Finished " + sid.storyName.get)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          println("Skipping " + sid.storyName.get + " (ERROR)")
      }
    } else {
      println("Skipping " + sid.storyName.get + " (Already processed)")
    }
  })

}

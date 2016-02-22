package annotation

import java.io.File

import io.Source._


/**
 * Trait for any type of brat annotation.
 */
trait BratAnnotation

/**
 * Container class for a single brat entity annotation.
 * @param id Annotation Id.
 * @param tag Tag (as string).
 * @param startCharacterOffset Starting character offset of entity span.
 * @param endCharacterOffset Ending character offset of entity span.
 * @param span Textual span of entity.
 */
case class BratEntityAnnotation(id: String,
                                tag: String,
                                startCharacterOffset: Int,
                                endCharacterOffset: Int,
                                span: String) extends BratAnnotation

/**
 * Container class for the brat annotations contained in the .ann file located at the given filepath.
 * @param filepath Filepath to .ann file.
 */
class BratAnnotations(filepath: String) {
  import BratAnnotations._

  /**
   * The Id of the given brat annotations.
   * @return Id.
   */
  def id: String = getId(this.filepath)

  /**
   * List of all annotations contained in the given .ann file.
   */
  val annotations: List[BratAnnotation] = loadAnnotations(this.filepath)

}

object BratAnnotations {

  /** File extension for brat annotation files. */
  val FILE_EXT = ".ann"

  // Retrieves the Id for the brat annotaton file (.ann file) located at the given filepath (filename).
  private def getId(filepath: String): String = new File(filepath).getName.split("\\.")(0)

  // Loads the annotations as a list from the .ann file located at the given filepath.
  private def loadAnnotations(filepath: String): List[BratAnnotation] = {
    // Extracted the entity annotaton in the given line from the .ann file.
    def lineToEntityAnnotation(line: String): BratEntityAnnotation = {
      val entries: Array[String] = line.split("\t")

      val id = entries(0)
      val span = entries(2)

      val subEntries: Array[String] = entries(1).split(" ")

      val tag = subEntries(0)
      val startCharacterOffset = subEntries(1).toInt

      var endCharacterOffset = -1
      try {
        endCharacterOffset = subEntries(2).toInt
      } catch {
        case e: NumberFormatException => endCharacterOffset = subEntries(3).toInt
      }

      BratEntityAnnotation(id, tag, startCharacterOffset, endCharacterOffset, span)
    }

    fromFile(new File(filepath)).getLines().map(lineToEntityAnnotation).toList
  }

}

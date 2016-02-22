package annotation.document

import java.io.FileWriter

/**
 * Document to annotate.
 * @param preWindow Pre-window list of text segments (Segments preceding annotation area).
 * @param area List of text segments in annotation area (Segments in area to annotate).
 * @param postWindow Post-window list of text segments (Segments following annotation area).
 */
case class AnnotationDocument(preWindow: List[String], area: List[String], postWindow: List[String]) {
  import AnnotationDocument._

  /**
   * Returns the number of segments in the annotation area.
   * @return Number of area segments.
   */
  def numAreaSegments: Int = this.area.length

  /**
   * Outputs the annotation document to the file located at the given filepath (The pre-window followed by
   * ANNOT_AREA START followed by the area followed by ANNOT_AREA_END followed by the post-window).
   * @param filepath Output filepath.
   * @param separator Separator for segments (Default is SEG_SEPARATOR).
   */
  def toFile(filepath: String, separator: String = SEG_SEPARATOR): Unit = {
    val fw = new FileWriter(filepath)
    fw.write((this.preWindow ++ List(ANNOT_AREA_START) ++ this.area ++ List(ANNOT_AREA_END) ++
      this.postWindow).mkString(separator))
    fw.close()
  }

}

object AnnotationDocument {

  /** Default separator of segments in annotation document text. */
  val SEG_SEPARATOR = "\n\n"

  /** Denotes start of annotation area in annotation document text. */
  val ANNOT_AREA_START = "<===== BEGIN ANNOTATION =====>\n------------------------------"
  /** Denotes end of annotation area in annotation document text. */
  val ANNOT_AREA_END = "----------------------------\n<===== END ANNOTATION =====>"

}
package annotation.document.runners

import java.io.File

import annotation.document.AnnotationDocumentGenerator
import story.PrideAndPrejudice


/**
 * Generates annotation documents for all of Pride and Prejudice.
 */
private object GeneratePrideAndPrejudiceAnnotationDocuments extends App {

  // Path to output directory for annotation documents.
  val ANNOT_DOC_DIRPATH = "data/annotations/alias-ident/pride-and-prejudice"

  // Number of words for the pre-window in each document.
  val PREWINDOW_LENGTH = 100
  // Number of words for the annotation area in each document.
  val AREA_LENGTH = 250
  // Number of words for the post-window in each document.
  val POSTWINDOW_LENGTH = 100

  var chapterNumber = 0
  // Generate documents for each chapter
  for (chapterNumber <- 1 to PrideAndPrejudice.numChapters) {
    System.out.println("Chapter " + chapterNumber)

    val chapterDir = new File(ANNOT_DOC_DIRPATH + File.separator + "chapter-" + chapterNumber)
    // Create output sub-directory for the given chapter.
    if (!chapterDir.exists()) {
      chapterDir.mkdirs()
      System.out.println("(Created " + chapterDir.getPath + ")")
    }

    val chapterParagraphs: Array[String] = PrideAndPrejudice.getChapterParagraphs(chapterNumber)
    val numChapterParagraphs = chapterParagraphs.length

    // Generate documents with non-overlapping annotation areas that cover all the chapter text.
    var annotationAreaStartIndex = 0
    while (annotationAreaStartIndex < numChapterParagraphs) {
      val annotationDocument = AnnotationDocumentGenerator.generate(chapterParagraphs, annotationAreaStartIndex,
        PREWINDOW_LENGTH, AREA_LENGTH, POSTWINDOW_LENGTH)
      annotationDocument.toFile(chapterDir.getPath + File.separator + annotationAreaStartIndex + ".txt")

      System.out.println("Created " + annotationAreaStartIndex + ".txt")

      annotationAreaStartIndex += annotationDocument.numAreaSegments
    }
  }

}

package annotation.document

import utils.Utils

/**
 * Generates documents from story text for annotation.
 */
object AnnotationDocumentGenerator {

  // Generates the pre-window just before the given start area index, comprising of the minimum number of segments
  // immediately preceding the area, with at least preWindowLength number of words.
  private def getPreWindow(textSegments: Array[String], areaStartIndex: Int, preWindowLength: Int): List[String] = {
    var preWindow: List[String] = List()

    var wordCount = 0
    var i = areaStartIndex - 1
    while (wordCount < preWindowLength && i >= 0) {
      preWindow :+= textSegments(i)

      wordCount += Utils.wordCount(textSegments(i))
      i -= 1
    }

    preWindow.reverse
  }

  // Generates the annotation area at the given start area index, comprising of the minimum number of successive
  // segments starting at that index, with at least areaLength number of words.
  private def getArea(textSegments: Array[String], areaStartIndex: Int, areaLength: Int): List[String] = {
    var area: List[String] = List()

    val numTextSegments = textSegments.length

    var wordCount = 0
    var i = areaStartIndex
    while (wordCount < areaLength && i < numTextSegments) {
      area :+= textSegments(i)

      wordCount += Utils.wordCount(textSegments(i))
      i += 1
    }

    area
  }

  // Generates the post-window at the given start post-window index, comprising of the minimum number of successive
  // segments starting at that index, with at least postWindowLength number of words.
  private def getPostWindow(textSegments: Array[String],
                            postWindowStartIndex: Int,
                            postWindowLength: Int): List[String] = {
    var postWindow: List[String] = List()

    val numTextSegments = textSegments.length

    var wordCount = 0
    var i = postWindowStartIndex
    while (wordCount < postWindowLength && i < numTextSegments) {
      postWindow :+= textSegments(i)

      wordCount += Utils.wordCount(textSegments(i))
      i += 1
    }

    postWindow
  }

  /**
   * Generates an annotation document from the given array of text segments.
   * @param textSegments Array of text segments to generate a document from.
   * @param areaStartIndex Index (in the array of text segments) of the starting segment in the annotation area.
   * @param preWindowLength Number of words to include in the pre-window.
   * @param areaLength Number of words to include in the annotation area.
   * @param postWindowLength Number of words to include in the post-window.
   * @return Annotation document.
   */
  def generate(textSegments: Array[String],
               areaStartIndex: Int,
               preWindowLength: Int,
               areaLength: Int,
               postWindowLength: Int): AnnotationDocument = {
    val preWindow: List[String] = getPreWindow(textSegments, areaStartIndex, preWindowLength)
    val area: List[String] = getArea(textSegments, areaStartIndex, areaLength)
    val postWindow: List[String] = getPostWindow(textSegments, areaStartIndex + area.size, postWindowLength)

    AnnotationDocument(preWindow, area, postWindow)
  }

}

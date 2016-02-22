package annotation


/**
 * Represents a single Frankenbrat character annotation.
 * @param id Annotation id.
 * @param tag Character tag.
 * @param startOffset Starting character offset.
 * @param endOffset Ending character offset.
 */
case class FrankenbratAnnotation(id: String,
                                 tag: String,
                                 startOffset: Int,
                                 endOffset: Int) {

  override def equals(o: Any): Boolean = o match {
    case a: FrankenbratAnnotation => this.tag == a.tag && this.startOffset == a.startOffset &&
      this.endOffset == a.endOffset
    case _ => false
  }

  override def hashCode: Int = (this.tag, this.startOffset, this.endOffset).hashCode()

}

/**
 * Represents the Frankenbrat character annotations for a single document.
 * @param id Annotation id.
 * @param annotations Sequence of Frankenbrat character annotations.
 */
case class FrankenbratDocAnnotations(id: String, annotations: Seq[FrankenbratAnnotation])

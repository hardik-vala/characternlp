package annotation.character

import java.io.File

import annotation.{FrankenbratDocAnnotations, FrankenbratAnnotation}


/**
 * Container class for a single typed alias annotation.
 * @param id (brat) Annotation Id.
 * @param tag Tag/Type (as string).
 * @param startCharacterOffset Starting character offset of entity span.
 * @param endCharacterOffset Ending character offset of entity span.
 */
case class CharacterAnnotation(id: String,
                               tag: String,
                               startCharacterOffset: Int,
                               endCharacterOffset: Int) {

  // Alternative constructor from a character Frankenbrat annotation,
  def this(fa: FrankenbratAnnotation) = this(fa.id, fa.tag, fa.startOffset, fa.endOffset)

  override def equals(o: Any): Boolean = o match {
    case ca: CharacterAnnotation => this.tag == ca.tag && this.startCharacterOffset == ca.startCharacterOffset &&
      this.endCharacterOffset == ca.endCharacterOffset
    case _ => false
  }

  override def hashCode: Int = (this.tag, this.startCharacterOffset, this.endCharacterOffset).hashCode

  /**
   * Returns the character offsets as a pair.
   * @return Starting and ending character offsets as a pair (in that order).
   */
  def offsets: (Int, Int) = (this.startCharacterOffset, this.endCharacterOffset)

  override def toString: String = this.tag + "[" + this.startCharacterOffset + "-" + this.endCharacterOffset + "]"

  /**
   * Tests equality with another character annotation on the character offset fields
   * @param ca Other character annotation to test equality with.
   * @return True if the character offset fields match, false otherwise.
   */
  def untypedEquals(ca: CharacterAnnotation): Boolean = this.startCharacterOffset == ca.startCharacterOffset &&
    this.endCharacterOffset == ca.endCharacterOffset

}

/**
 * Container class for the character annotations for a given text.
 * @param id The Id of the given character annotations.
 * @param annotations Sequence of character annotations in the text.
 */
case class CharacterAnnotations(id: String, annotations: Seq[CharacterAnnotation]) {

  /**
   * Alternative constructor from Frankenbrat annotations for a given document.
   * @param fba Frankenbrat annotations for the document.
   * @return CharacterAnnotations.
   */
  def this(fba: FrankenbratDocAnnotations) = this(fba.id, fba.annotations.map(new CharacterAnnotation(_)))

  /**
   * Returns a sequence of characters annotations with the given tag.
   * @param tag Tag.
   * @return sequence of characters annotations with the given tag.
   */
  def get(tag: String): Seq[CharacterAnnotation] = this.annotations.filter(_.tag == tag)

  /**
   * Returns the number of individual annotations.
   * @return Number of annotations.
   */
  def size: Int = this.annotations.size

  /**
   * Returns the sequence of character annotations corresponding to group aliases.
   * @return Sequence of character annotations corresponding to group aliases.
   */
  def group: Seq[CharacterAnnotation] = {
    // Set of character offsets belonging to character annotations that are part of a group.
    val groupOffsets: Set[(Int, Int)] =
      this.annotations.groupBy(_.offsets).filter({ case (offsets, cas) => cas.size > 1}).keySet
    this.annotations.filter(ca => groupOffsets.contains(ca.offsets))
  }

  /**
   * Intersections the given set of character annotation with annother
   * @param ca Other character annotations.
   * @return List of shared character annotations.
   */
  def &(ca: CharacterAnnotations): List[CharacterAnnotation] = (this.annotations.toSet & ca.annotations.toSet).toList

  /**
   * Returns a list of those character annotations not contained in another set of annotations.
   * @param ca Other character annotations.
   * @return List of character annotations not contained in the other.
   */
  def &~(ca: CharacterAnnotations): List[CharacterAnnotation] = (this.annotations.toSet &~ ca.annotations.toSet).toList

}

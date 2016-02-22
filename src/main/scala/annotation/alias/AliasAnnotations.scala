package annotation.alias

import java.io.File

import annotation.{BratEntityAnnotation, BratAnnotations}


/**
 * Container class for a single untyped alias annotation.
 * @param id (brat) Annotation Id.
 * @param startCharacterOffset Starting character offset of entity span.
 * @param endCharacterOffset Ending character offset of entity span.
 * @param span Textual span of entity.
 */
case class UntypedAliasAnnotation(id: String,
                           startCharacterOffset: Int,
                           endCharacterOffset: Int,
                           span: String) {

  override def equals(o: Any): Boolean = o match {
    case aa: UntypedAliasAnnotation => this.startCharacterOffset == aa.startCharacterOffset &&
      this.endCharacterOffset == aa.endCharacterOffset && this.span == aa.span
    case _ => false
  }

  override def hashCode: Int = (this.startCharacterOffset, this.endCharacterOffset, this.span).hashCode

  override def toString: String = this.span + "[" + this.startCharacterOffset + "-" + this.endCharacterOffset + "]"

}

/**
 * Container class for a single typed alias annotation.
 * @param id (brat) Annotation Id.
 * @param tag Tag/Type (as string).
 * @param startCharacterOffset Starting character offset of entity span.
 * @param endCharacterOffset Ending character offset of entity span.
 * @param span Textual span of entity.
 */
case class TypedAliasAnnotation(id: String,
                           tag: String,
                           startCharacterOffset: Int,
                           endCharacterOffset: Int,
                           span: String) {

  override def equals(o: Any): Boolean = o match {
    case taa: TypedAliasAnnotation => this.tag == taa.tag && this.startCharacterOffset == taa.startCharacterOffset &&
      this.endCharacterOffset == taa.endCharacterOffset && this.span == taa.span
    case _ => false
  }

  override def hashCode: Int = (this.tag, this.startCharacterOffset, this.endCharacterOffset, this.span).hashCode

  override def toString: String = this.span + " (" + this.tag + ") " + "[" + this.startCharacterOffset + "-" +
    this.endCharacterOffset + "]"

  /**
   * Tests equality with another typed alias annotations on the span and character offset fields
   * @param taa Other typed alias annotation to test equality with.
   * @return True if span and character offset fields match, false otherwise.
   */
  def untypedEquals(taa: TypedAliasAnnotation): Boolean = this.startCharacterOffset == taa.startCharacterOffset &&
    this.endCharacterOffset == taa.endCharacterOffset && this.span == taa.span

}

/**
 * Container class for the alias annotations for a given text.
 * @param filepath Filepath to corresponding brat annotation file (i.e. .ann file).
 */
class AliasAnnotations(filepath: String) {
  import AliasAnnotations._

  /**
   * The Id of the given alias annotations.
   * @return Id.
   */
  def id: String = getId(this.filepath)

  /**
   * A list of the typed alias annotations.
   */
  val typedAnnotations: List[TypedAliasAnnotation] = toTypedAliasAnnotations(new BratAnnotations(filepath))

  /**
   * Returns a list of the untyped alias annotations.
   * @return List of untyped alias annotations.
   */
  def untypedAnnotations: List[UntypedAliasAnnotation] = toUnTypedAliasAnnotations(this.typedAnnotations)

  /**
   * Returns a list of (typed) alias annotations with given alias type.
   * @param aliasType Alias type.
   * @return List of typed alias annotations with given alias type.
   */
  def get(aliasType: String): List[TypedAliasAnnotation] = this.typedAnnotations.filter(_.tag == aliasType)

  /**
   * Returns the number of individual annotations.
   * @return Number of annotations.
   */
  def size: Int = this.typedAnnotations.size

}

object AliasAnnotations {

  // Retrieves the Id for the alias annotatons contained in the brat .ann file located at the given filepath (filename).
  private def getId(filepath: String): String = {
    val filename = (new File(filepath)).getName
    filename.substring(0, filename.lastIndexOf('.'))
  }

  // TODO
  private def toTypedAliasAnnotations(bratAnnotations: BratAnnotations): List[TypedAliasAnnotation] = {
    def toTypedAliasAnnotation(bea: BratEntityAnnotation) =
      TypedAliasAnnotation(bea.id, bea.tag, bea.startCharacterOffset,bea.endCharacterOffset, bea.span)

    bratAnnotations.annotations.map(ba => toTypedAliasAnnotation(ba.asInstanceOf[BratEntityAnnotation]))
  }

  // TODO
  private def toUnTypedAliasAnnotations(typedAliasAnnotations: List[TypedAliasAnnotation]):
    List[UntypedAliasAnnotation] = {

    def toUntypedAliasAnnotation(taa: TypedAliasAnnotation): UntypedAliasAnnotation =
      UntypedAliasAnnotation(taa.id, taa.startCharacterOffset, taa.endCharacterOffset, taa.span)

    typedAliasAnnotations.map(toUntypedAliasAnnotation)
  }
}

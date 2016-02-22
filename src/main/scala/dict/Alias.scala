package dict

import name.Gender
import utils.Utils


/**
 * Alias type.
 */
trait AliasType

/**
 * Proper alias type.
 * @param subType Sub-type.
 */
case class Proper(subType: ProperSubType) extends AliasType
/**
 * Pseudonym alias type.
 * @param subType Sub-type.
 */
case class Pseudonym(subType: PseudonymSubType) extends AliasType
/**
 * Role alias type.
 * @param subType Sub-type.
 */
case class Role(subType: RoleSubType) extends AliasType
/**
 * UGA alias type.
 */
case object UGA extends AliasType
/**
 * Group alias type.
 * @param subType Sub-type.
 */
case class Group(subType: GroupSubType) extends AliasType

/**
 * Alias sub-type.
 */
trait AliasSubType

/**
 * Proper sub-type.
 */
trait ProperSubType extends AliasSubType
case object FULL extends ProperSubType
case object PARTIAL extends ProperSubType

/**
 * Pseudonym sub-type.
 */
trait PseudonymSubType extends AliasSubType
case object NICKNAME extends PseudonymSubType
case object REGNAL extends PseudonymSubType
case object VIRTUAL extends PseudonymSubType

/**
 * Role sub-type.
 */
trait RoleSubType extends AliasSubType
case object KIN extends RoleSubType
case object SOCIAL extends RoleSubType
case object JOB extends RoleSubType

/**
 * Group sub-type.
 */
trait GroupSubType extends AliasSubType
case object ORGANIZATION extends GroupSubType
case object GENERIC extends GroupSubType

/**
 * Alias annotation container.
 * @param identity Identity #.
 * @param aliasType Alias type.
 * @param titled True if titled, false otherwise.
 * @param abbrev True if abbreviated, false otherwise.
 * @param qualified True if qualified, false otherwise.
 */
case class AliasAnnotation(identity: Int, aliasType: AliasType, titled: Boolean, abbrev: Boolean, qualified: Boolean)


//sealed trait AliasLike {
//  val span: String
//  val annotation: Option[AliasAnnotation]
//
//  // Equality is based on spans.
//  override def equals(o: Any): Boolean = o match {
//    case a: Alias => this.span == a.span
//    case _ => false
//  }
//
//  override def hashCode: Int = this.span.hashCode
//
//}

/**
 * Alias.
 * @param span Textual span of alias.
 * @param annotation Optional alias annotation.
 */
case class Alias(span: String, annotation: Option[AliasAnnotation]) {

  // Overloaded constructor for no accompanying annotation.
  def this(span: String) = this(span, None)

  // Equality is based on spans.
  override def equals(o: Any): Boolean = o match {
    case a: Alias => this.span == a.span
    case _ => false
  }

  override def hashCode: Int = this.span.hashCode

}

object Alias {

  /**
   * Returns a comparator, comparing the alias spans of a1 and a2 lexicographically.
   * @param a1 First alias.
   * @param a2 Second alias.
   * @return True if the span of a1 is lexicographically before or the same as the span of a2, false otherwise.
   */
  def aliasSpanComparator(a1: Alias, a2: Alias) = a1.span <= a2.span

  /**
   * Returns a comparator, comparing the alias spans of a1 and a2 based on length. If they're of equal length, then
   * the spans are compared lexicographically.
   * @param a1 First alias.
   * @param a2 Second alias.
   * @return True if the span of a1 is larger than the span for a2. If they're of equal length, then it returns true if
   *         the span of a1 appears lexicographically before the span of a2, and false otherwise.
   */
  def aliasSpanLengthComparator(a1: Alias, a2: Alias) = Utils.stringLengthComparator(a1.span, a2.span)

}

/**
 * Gendered alias.
 * @param span Textual span of alias.
 * @param gender Gender.
 * @param annotation Optional alias annotation.
 */
class GenderedAlias(span: String,
                    val gender: Gender,
                    annotation: Option[AliasAnnotation]) extends Alias(span, annotation) {

  // Overloaded constructor for no accompanying annotation.
  def this(span: String, gender: Gender) = this(span, gender, None)

  override def toString: String = "GenderedAlias(" + this.span + "," + this.gender.name + ")"

}

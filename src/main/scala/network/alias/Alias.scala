package network.alias

import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import network.alias.AliasType.AliasType


/**
 * Enumeration of alias types.
 */
object AliasType extends Enumeration {

  type AliasType = Value
  val DESCRIPTIVE = Value("Descriptive")
  val FUNCTIONAL = Value("Functional")
  val PRONOUN = Value("Pronoun")
  val PROPER = Value("Proper")
  val RELATIONAL = Value("Relational")
  val TITLED = Value("Titled")
  val UNKNOWN = Value("UNKNOWN")

}

/**
 * Alias.
 * @param span Alias span.
 * @param startOffset Starting character offset of alias span.
 * @param endOffset Ending character offset of alias span.
 * @param aliasType Optional alias type.
 */
case class Alias(span: String, startOffset: Int, endOffset: Int, aliasType: Option[AliasType]) extends Ordered[Alias] {
  import scala.math.Ordered.orderingToOrdered

  /**
   * Alternate constructor that sets the alias type to None.
   * @param span Alias span.
   * @param startOffset Starting character offset of alias span.
   * @param endOffset Ending character offset of alias span.
   * @return Alias with None alias type.
   */
  def this(span: String, startOffset: Int, endOffset: Int) = this(span, startOffset, endOffset, None)

  override def equals(o: Any): Boolean = o match {
    case a: Alias => this.span == a.span && this.startOffset == a.startOffset && this.endOffset == a.endOffset
    case _ => false
  }

  override def hashCode: Int = (this.span, this.startOffset, this.endOffset).hashCode()

  override def compare(that: Alias): Int =
    (this.startOffset, this.endOffset, this.span) compare (that.startOffset, that.endOffset, that.span)

  override def toString: String = this.aliasType match {
    case Some(at) => this.span + " (" + at + ")" + " [" + this.startOffset + "," + this.endOffset + "]"
    case None => this.span + " [" + this.startOffset + "," + this.endOffset + "]"
  }

}

object Aliases {

  /**
   * Loads a list of aliases from a .csv file.
   * @param filepath Filepath to .csv file.
   * @return List of aliases.
   */
  def fromCSV(filepath: String): List[Alias] =
    CSVReader.open(new File(filepath)).all().map(row =>
      Alias(row(0), row(1).toInt, row(2).toInt, if (row.size > 3) Some(AliasType.withName(row(3))) else None))

  /**
   * Saves the given list of aliases to a .csv file.
   * @param aliases Iterable of aliases.
   * @param filepath Filepath to .csv file.
   */
  def toCSV(aliases: Iterable[Alias], filepath: String): Unit =
    CSVWriter.open(new File(filepath)).writeAll(aliases.map(a =>
      List(a.span, a.startOffset, a.endOffset) ++ (if (a.aliasType.isDefined) List(a.aliasType.get) else List())).toSeq)

}

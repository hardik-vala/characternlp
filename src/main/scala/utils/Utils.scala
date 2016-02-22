package utils

import java.io.{File, FilenameFilter}


/**
 * Container for utility functions.
 */
object Utils {

  /**
   * Returns the filename of the file located by the given filepath, without the extension.
   * @param filepath Filepath.
   * @return Filename without the extension.
   */
  def getFilenameWithNoExt(filepath: String): String = {
    val filename = new File(filepath).getName
    filename.substring(0, filename.lastIndexOf('.'))
  }

  /**
   * Returns a filename filter for excluding hidden files.
   * @return Filename filter that excludes hidden files.
   */
  def noHiddenFilenameFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !name.startsWith(".")
  }

  /**
   * Returns an array of files recursively found in traversing the given directory.
   * @param dir Directory to traverse.
   * @return Array of files.
   */
  def recursiveListFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    files ++ files.filter(_.isDirectory).flatMap(d => recursiveListFiles(d))
  }

  /**
   * Returns an array of files recursively found in traversing the fiven directory that matches the given filename
   * filter.
   * @param dir Directory to traverse.
   * @param filenameFilter Filename filter.
   * @return Array of files.
   */
  def recursiveListFiles(dir: File, filenameFilter: FilenameFilter): Array[File] = {
    dir.listFiles(filenameFilter).filterNot(_.isDirectory) ++
      dir.listFiles().filter(_.isDirectory).flatMap(d => recursiveListFiles(d, filenameFilter))
  }

  /**
   * Checks if the given string is lowercase.
   * @param s String to check.
   * @return True if the string is all lowercase, false otherwise.
   */
  def isLowerCase(s: String): Boolean = s == s.toLowerCase()

  /**
   * Returns a comparator, comparing strings s1 and s2 based on length. If they're of equal length, then compares them
   * lexicographically.
   * @param s1 First string.
   * @param s2 Second string.
   * @return True if s1 has larger length than s2, otherwise false. But if they're of equal length, then it returns true
   *         if s1 appears lexicographically before s2, and false otherwise.
   */
  def stringLengthComparator(s1: String, s2: String) = if (s1.length == s2.length) s1 < s2 else s2.length < s1.length

  /**
   * Returns the word count of a string (Splitting on whitespace).
   * @param s String.
   * @return Word count.
   */
  def wordCount(s: String): Int = s.split("\\s+").length

  /**
   * Counts the phrase occurrences for each of the given phrases in the given text.
   * @param phrases Phrases.
   * @param text Text.
   * @return Mapping from phrases to counts.
   */
  def phraseCounts(phrases: Iterable[String], text: String): Map[String, Int] = {
    val countedToken: String = " <COUNTED> "

    var counts: Map[String, Int] = Map()
    var t: String = text.split("\\s+").mkString(" ")

    phrases.toList.sortBy(-_.length).foreach(p => {
      if (p.contains("*"))
        counts += (p -> 0)
      else {
        counts += (p -> ("\\W" + p + "\\W").r.findAllMatchIn(t).length)
        t = ("\\W" + p + "\\W").r.replaceAllIn(t, countedToken)
      }
    })

    counts
  }

  /**
   * Checks if a given sequence is a sub-sequence of another.
   * @param seq1 A sequence.
   * @param seq2 Sequence to check for seq1.
   * @tparam A Common type of sequence elements.
   * @return True if seq1 is a sub-sequence of seq2, false otherwise.
   */
  def isSubSeq[A](seq1: Seq[A], seq2: Seq[A]): Boolean = seq1 match {
    case Nil => true
    case h1::t1 => seq2 match {
      case Nil => false
      case h2::t2 => if (h1 == h2) isSubSeq(t1, t2) else isSubSeq(seq1, t2)
    }
  }

}

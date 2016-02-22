package dict


/**
 * Character as a set of aliases.
 * @param aliases Set of aliases.
 */
case class Character(aliases: Set[Alias]) {

  /**
   * Returns the set of common aliases between the given character and another.
   * @param c Other character.
   * @return Set of common aliases.
   */
  def &(c: Character): Set[Alias] = this.aliases & c.aliases

  /**
   * Returns the set of all distinct aliases between the given character and another.
   * @param c Other character.
   * @return Set of all distinct aliases.
   */
  def |(c: Character): Set[Alias] = this.aliases | c.aliases

  // Equality is based on the equality of the sets of aliases.
  override def equals(o: Any): Boolean = o match {
    case c: Character => this.aliases.equals(c.aliases)
    case _ => false
  }

  override def hashCode: Int = this.aliases.hashCode

  /**
   * Returns a list of aliases in terms of their spans.
   * @param sorted True if the returned list should be sorted alphabetically, false otherwise.
   * @return List of alias spans.
   */
  def getAliasSpans(sorted: Boolean = true) =
    if (sorted) this.aliases.map(_.span).toList.sorted else this.aliases.map(_.span).toList

  /**
   * Returns the size of the character, i.e. the number of aliases belonging the character.
   * @return Size of the character.
   */
  def size: Int = this.aliases.size

  override def toString: String = "[" + this.getAliasSpans(sorted = true).mkString(", ") + "]"

}

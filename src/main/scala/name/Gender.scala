package name


/**
 * Gender.
 */
sealed trait Gender {

  /**
   * Gender as a string.
   * @return Gender string.
   */
  def name: String

  override def toString: String = this.name

}

/** Female gender. */
case object FEMALE extends Gender { override def name: String = "FEMALE" }

/** Male gender. */
case object MALE extends Gender { override def name: String = "MALE" }

/** Unknown gender. */
case object UNKNOWN extends Gender { override def name: String = "UNKNOWN" }

object Gender {

  /**
   * Assigns a gender to the given span.
   * @param span Span.
   * @return Assigned gender.
   */
  def assign(span: String): Gender = {
    val hasFemaleTitle = NameTitles.hasFemaleTitle(span)
    val hasMaleTitle = NameTitles.hasMaleTitle(span)
    val hasStrictFemaleName = NameGazetteer.hasStrictFemaleName(span)
    val hasStrictMaleName = NameGazetteer.hasStrictMaleName(span)

    if (hasFemaleTitle && !hasMaleTitle)
      FEMALE
    else if (hasMaleTitle && !hasFemaleTitle)
      MALE
    else if (hasStrictFemaleName && !hasStrictMaleName)
      FEMALE
    else if (hasStrictMaleName && !hasStrictFemaleName)
      MALE
    else
      UNKNOWN
  }

}

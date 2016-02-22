package name

import scala.util.matching.Regex


/**
 * Manages all name titles in the English language.
 */
object NameTitles {

  /**
   * Returns a set of all name titles.
   * @return Set of all name titles.
   */
  def get: Set[String] = Set(
    // Generic titles. 
    "Mr.",
    "Monsieur",
    "Signor",
    "Mrs.",
    "Ms.",
    "Miss",
    "Mx.",
    "Mister",
    "Master",
    "Maid",
    "Madam",
    "Madame",
    "Mme.",
    "Aunt",
    "Auntie",
    "Uncle",
    "Gentlelady",
    "Gentlewoman",
    "Gentleman",

    // Legislative and executive titles.
    "Honourable",
    "Hon.",
    "Right Honourable",
    "Rt. Hon.",
    "MP",
    "MYP",
    "Representative",
    "Senator",
    "Speaker",
    "President",
    "Deputy President",
    "Executive Vice President",
    "Lord President",
    "Lord President of the Council",
    "Vice President",
    "Councillor",
    "Alderman",
    "Selectman",
    "Delegate",
    "Mayor",
    "Lord Mayor",
    "Lady Mayoress",
    "Governor",
    "Lieutenant Governor",
    "Prefect",
    "Prelate",
    "Premier",
    "Burgess",
    "Ambassador",
    "Envoy",
    "Secretary",
    "Cardinal Secretary of State",
    "Foreign Secretary",
    "General Secretary",
    "Secretary of State",
    "Minister",
    "Cardinal Minister of State",
    "Foreign Minister",
    "General Minister",
    "Minister of State",
    "Provost",

    // Aristocratic titles.
    "Prince",
    "Princess",
    "Archduke",
    "Archduchess",
    "Grand Duke",
    "Grand Duchess",
    "Duke",
    "Duchess",
    "Marquis",
    "Marquess",
    "Count",
    "Countess",
    "Earl",
    "Viscount",
    "Lord",
    "Lady",
    "Emperor",
    "Empress",
    "King",
    "Queen",
    "Tsar",
    "Tsarina",
    "Leader",
    "Baron",
    "Baroness",
    "Pope",
    "Popess",
    "Viceroy",
    "Vicereine",

    // Titles used by knights, dames, baronets and baronetesses.
    "Sir",
    "Dame",
    "Chevalier",

    // Judicial titles.
    "Advocate",
    "Advocate General",
    "Attorney",
    "Bailiff",
    "Barrister",
    "Chancellor",
    "Chancellor of the High Court",
    "Congressman",
    "Congresswoman",
    "Judge",
    "Admiralty Judge",
    "Justice",
    "Lord Chief Justice",
    "Lord Justice Clerk",
    "Lord Justice of Appeal",
    "Justice of the Peace",
    "Magistrate",
    "Promagistrate",
    "Master of the Rolls",
    "Member",
    "Chairman",
    "Mufti",
    "Grand Mufti",
    "Lord President of the Court of Session",
    "Privy Counsellor",
    "Privy Councillor",
    "Queen's Counsel",
    "King's Counsel",
    "Solicitor",

    // Historical titles.
    "Lictor",
    "Reeve",
    "Seneschal",
    "Tribune",

    // Religious titles.
    "Abbess",
    "Abbot",
    "Brother",
    "Sister",
    "Mother Superior",
    "Friar",
    "Mother",
    "Mother Superior",
    "Reverend Mother",
    "Bishop",
    "Archbishop",
    "Boy Bishop",
    "Lord Archbishop",
    "Metropolitan Bishop",
    "Prince Bishop",
    "Presbyter",
    "Priest",
    "High Priest",
    "Priestess",
    "Father",
    "Fr.",
    "Reverend",
    "Reverend Mr.",
    "Patriarch",
    "Catholicos",
    "Vicar",
    "Chaplain",
    "Canon",
    "Pastor",
    "Prelate",
    "Primate",
    "Dom",
    "Cardinal",
    "Ter",
    "Servant of God",
    "Venerable",
    "Blessed",
    "Saint",
    "S.",
    "St.",
    "Deacon",
    "Archdeacon",
    "Acolyte",
    "Elder",
    "Monsignor",
    "Reader",
    "Almoner",
    "Lord High Almoner",

    // Academic titles.
    "Dr.",
    "Prof.",
    "Professor",

    // Military titles.
    "Colonel",
    "General",
    "Commodore",
    "Air Commodore",
    "Corporal",
    "Lance Corporal",
    "Staff Corporal",
    "Mate",
    "Chief Mate",
    "First Mate",
    "Inspector",
    "Sergeant",
    "Sergeant at Mace",
    "Sergeant of Arms",
    "Admiral",
    "Grand Admiral",
    "Lord High Admiral",
    "Rear Admiral",
    "Vice Admiral",
    "Brigadier",
    "Captain",
    "Group Captain",
    "Commander",
    "Commander-in-Chief",
    "Lieutenant Commander",
    "Wing Commander",
    "General",
    "Adjutant General",
    "Attorney General",
    "Captain General",
    "Colonel General",
    "Director General",
    "Generalissimo",
    "General of the Army",
    "Governor General",
    "Lieutenant General",
    "Lord Justice General",
    "Major General",
    "Resident General",
    "Secretary General",
    "Solicitor General",
    "Surgeon General",
    "Vicar General",
    "Officer",
    "Air Officer",
    "Chief Academic Officer",
    "Chief Analytics Officer",
    "CAO",
    "Chief Business Development Officer",
    "CBDO",
    "Chief Credit Officer",
    "CCO",
    "Chief Executive Officer",
    "CEO",
    "Chief Financial Officer",
    "CFO",
    "Chief Information Officer",
    "CIO",
    "Chief Information Security Officer",
    "CISO",
    "Chief Knowledge Officer",
    "CKO",
    "Chief Marketing Officer",
    "CMO",
    "Chief Operating Officer",
    "COO",
    "Chief Petty Officer",
    "CPO",
    "Chief Risk Officer",
    "CRO",
    "Chief Security Officer",
    "Chief Strategy Officer",
    "CSO",
    "Chief Technical Officer",
    "Chief Technology Officer",
    "CTO",
    "Chief Warrant Officer",
    "CWO",
    "Corporate Officer",
    "Customs Officer",
    "Field Officer",
    "First Officer",
    "Flag Officer",
    "Flying Officer",
    "General Officer",
    "Intelligence Officer",
    "Junior Warrant Officer",
    "Master Chief Petty Officer",
    "Master Warrant Officer",
    "Officer of State",
    "Petty Officer",
    "Pilot Officer",
    "Police Officer",
    "Political Officer",
    "Revenue Officer",
    "Senior Officer",
    "Ship's Officer",
    "Staff Officer",
    "Warrant Officer",
    "Lieutenant",
    "First Lieutenant",
    "Flight Lieutenant",
    "Lord Lieutenant",
    "Major",
    "Private",

    // Ranks of other organizations.
    "Constable",
    "Agent",

    // Unofficial use.
    "Principal",
    "Nurse",
    "Nanny",
    "Coach",
    "Wizard",
    "Chief Scout",
    "Queen's Scout",
    "Queen's Guide",
    "Scout",
    "Eagle Scout",
    "Citizen",
    "First Citizen",
    "Comrade"
  )

  /**
   * Returns a set of all female name titles.
   * @return Set of all female name titles.
   */
  def getFemale: Set[String] = Set(
    "Abbess",
    "Archduchess",
    "Aunt",
    "Auntie",
    "Baroness",
    "Congresswoman",
    "Countess",
    "Dame",
    "Duchess",
    "Empress",
    "Gentlelady",
    "Gentlewoman",
    "Grand Duchess",
    "Lady",
    "Madam",
    "Madame",
    "Mme.",
    "Maid",
    "Marquess",
    "Miss",
    "Mother",
    "Mrs.",
    "Ms.",
    "Popess",
    "Princess",
    "Queen",
    "Sister",
    "Tsarina",
    "Vicereine"
  )

  /**
   * Returns a set of all male name titles.
   * @return Set of all male name titles.
   */
  def getMale: Set[String] = Set(
    "Abbott",
    "Archduke",
    "Baron",
    "Brother",
    "Congressman",
    "Count",
    "Duke",
    "Earl",
    "Emperor",
    "Father",
    "Friar",
    "Gentleman",
    "Grand Duke",
    "King",
    "Lord",
    "Marquis",
    "Master",
    "Mister",
    "Monsieur",
    "Mr.",
    "Pope",
    "Prince",
    "Signor",
    "Sir",
    "Tsar",
    "Uncle",
    "Viceroy"
  )

  // Name title regex.
  private def titleRegex(t: String): Regex = new Regex(".*(\\s|^)" + t + "\\s.*")

  /**
   * Returns the set of titles contained in the given string.
   * @param s String.
   * @return Set of titles.
   */
  def getTitles(s: String): Set[String] = this.get.filterNot(t => this.titleRegex(t).findAllIn(s).isEmpty)

  /**
   * Checks if the given string is a name title.
   * @param s String to check.
   * @return True if the given string is a name title, false otherwise.
   */
  def isTitle(s: String): Boolean = this.get.contains(s)

  /**
   * Checks if the given string is a female name title.
   * @param s String to check.
   * @return True if the given string is a female name title, false otherwise.
   */
  def isFemaleTitle(s: String): Boolean = this.getFemale.contains(s)

  /**
   * Checks if the given string is a male name title.
   * @param s String to check.
   * @return True if the given string is a male name title, false otherwise.
   */
  def isMaleTitle(s: String): Boolean = this.getMale.contains(s)

  // Checks whether the given string contains a title from the list of name titles.
  private def hasTitle(s: String, titles: Iterable[String]): Boolean =
    titles.exists(t => s.matches(this.titleRegex(t).regex))

  /**
   * Checks whether the given string contains the given name title.
   * @param s String to check.
   * @param t Name title.
   * @return True if the string contains the given name title, false otherwise.
   */
  def hasTitle(s: String, t: String): Boolean = {
    require(this.isTitle(t), t + " is not a name title.")

    this.hasTitle(s, t::Nil)
  }

  /**
   * Checks whether the given string contains a name title.
   * @param s String to check.
   * @return True if the string contains a name title, false otherwise.
   */
  def hasTitle(s: String): Boolean = this.hasTitle(s, this.get)

  /**
   * Checks whether the given string contains a female name title.
   * @param s String to check.
   * @return True if the string contains a female name title, false otherwise.
   */
  def hasFemaleTitle(s: String): Boolean = this.hasTitle(s, this.getFemale)

  /**
   * Checks whether the given string contains a male name title.
   * @param s String to check.
   * @return True if the string contains a male name title, false otherwise.
   */
  def hasMaleTitle(s: String): Boolean = this.hasTitle(s, this.getMale)

  /**
   * Strips all name titles from the given string.
   * @param s String.
   * @return String with all titles stripped.
   */
  def stripTitles(s: String): String =
    this.get.fold (s) {case (acc, t) => acc.replaceAll("(\\s|^)" + t + "\\s", " ").trim}

}

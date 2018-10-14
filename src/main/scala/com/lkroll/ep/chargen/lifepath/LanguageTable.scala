package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{ Skill, Skills, Language, Languages }
import com.lkroll.ep.compendium.Aptitude

object LanguageTable extends Table with Pickable {
  import Languages._

  override type Result = Skill;

  private val data = utils.RollTable(
    (1 to 6) -> Arabic,
    (7 to 9) -> Bengali,
    (10 to 14) -> Cantonese, // Yue
    (15 to 15) -> Dutch,
    (16 to 24) -> English,
    (25 to 27) -> Farsi, // Persian
    (28 to 31) -> French,
    (32 to 35) -> German,
    (36 to 41) -> Hindi,
    (42 to 42) -> Italian,
    (43 to 47) -> Japanese,
    (48 to 51) -> Javanese,
    (52 to 53) -> Korean,
    (54 to 62) -> Mandarin,
    (63 to 63) -> Polish,
    (64 to 68) -> Portuguese,
    (69 to 71) -> Punjabi,
    (72 to 76) -> Russian,
    (77 to 78) -> Skandinaviska,
    (79 to 84) -> Spanish,
    (85 to 85) -> Swedish,
    (86 to 87) -> Tamil,
    (88 to 89) -> Turkish,
    (90 to 92) -> Urdu,
    (93 to 94) -> Vietnamese,
    (95 to 98) -> Wu,
    (99 to 100) -> Other);

  private val uncommonData = utils.RollTable(
    (1 to 5) -> Afrikaans,
    (6 to 10) -> Burmese,
    (11 to 15) -> Czech,
    (16 to 20) -> Estonian,
    (21 to 25) -> Finnish,
    (26 to 28) -> Greek,
    (29 to 32) -> Hebrew,
    (33 to 37) -> Indonesian,
    (38 to 40) -> Khmer,
    (41 to 45) -> Kurdish,
    (46 to 46) -> Latin,
    (47 to 50) -> Malay,
    (51 to 55) -> Romanian,
    (56 to 60) -> Serbian,
    (61 to 65) -> Slovak,
    (66 to 66) -> Suryan,
    (67 to 70) -> Swahili,
    (71 to 75) -> Tagalog,
    (76 to 85) -> Thai,
    (86 to 90) -> Xhosa,
    (91 to 95) -> Zulu,
    (96 to 97) -> Danish,
    (98 to 98) -> Icelandic,
    (99 to 100) -> Norwegian);

  override def label: String = "Random Langauge";
  override def source: String = "Transhuman p.43";
  override def roll(rand: Random): Result = {
    randomElement(rand).map(toSkill).get
  }

  override def numRows: Int = 100;
  override def pick(i: Int): Result = data.get(i).map(toSkill).get;

  private def randomElement(rand: Random): Option[Language] = {
    data.randomElement(rand).flatMap {
      case Other => uncommonData.randomElement(rand)
      case l     => Some(l)
    }
  }
  private def toSkill(lang: character.Language): Skill = Skills.Defaults.language.withField(lang.toString()).instance(0);
}

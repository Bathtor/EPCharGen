package com.lkroll.ep.chargen.impression

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.Implicits.RandomArray
import com.lkroll.ep.chargen.character.CharGenCharacter
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ GenderIdentity, MorphInstance }

case class Personality(style: String, voice: String) {
  def render: String = s"A $style personality, speaking in $voice";
}

class PersonalityTable(
  val genderIdentity: GenderIdentity,
  val actualAge:      Int,
  val currentMorph:   MorphInstance) extends Table {
  import PersonalityTable._;

  override type Result = Personality;

  lazy val dataVoicePitch: RollTableLike[String] = Array("High", "Medium", "Deep");
  lazy val dataVoiceTimbre: RollTableLike[String] = currentMorph.visibleGender match {
    case Some("Male") => RollTable(
      (1 to 90) -> "Male",
      (91 to 92) -> "Female",
      (93 to 95) -> "Monotone",
      (96 to 100) -> "Ambiguous")
    case Some("Female") => RollTable(
      (1 to 90) -> "Female",
      (91 to 92) -> "Male",
      (93 to 95) -> "Monotone",
      (96 to 100) -> "Ambiguous")
    case Some("Genderless") => RollTable(
      (1 to 60) -> "Ambiguous",
      (61 to 70) -> "Female",
      (71 to 80) -> "Male",
      (81 to 100) -> "Monotone")
    case Some(_) => RollTable(
      (1 to 3) -> "Male",
      (4 to 6) -> "Female",
      (7 to 8) -> "Monotone",
      (9 to 10) -> "Ambiguous")
    case None => Array("Male", "Female", "Monotone", "Ambiguous");
  }

  override def label: String = "Personality";
  override def source: String = "Lars Kroll";
  override def roll(rand: Random): Result = {
    val pitch = dataVoicePitch.randomElement(rand).get;
    val timbre = dataVoiceTimbre.randomElement(rand).get;
    val style = dataStyle.randomElement(rand).get;
    Personality(style, s"a $pitch voice with $timbre timbre")
  }
}

object PersonalityTable {
  def forChar(char: CharGenCharacter): PersonalityTable = new PersonalityTable(char.gender, char.age, char.activeMorph);

  val dataStyle: RollTableLike[String] = Array(
    "Doc (paternal)",
    "Grumpy (annoyed)",
    "Happy (cheerful)",
    "Sleepy (drowsy)",
    "Dopey (drugged/dumb)",
    "Bashful (shy)",
    "Sneezy (nasal/sickly)");
}

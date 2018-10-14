package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._

import Implicits.{ RandomArray, int2opt, str2opt };

class MorphInstantiation(
  val model:    MorphModel,
  val genderId: GenderIdentity.GenderIdentity) extends Table {
  import MorphInstantiation._;
  override type Result = MorphInstance;

  lazy val ageData: RollTableLike[Option[Int]] = model.name match {
    case MorphsDF.flat.name | MorphsS.splicer.name => RollTable(
      (1 to 25) -> 30,
      (26 to 50) -> 40,
      (51 to 60) -> 10,
      (61 to 70) -> 20,
      (71 to 75) -> 50,
      (81 to 85) -> 60,
      (86 to 90) -> 70,
      (91 to 94) -> 80,
      (95 to 97) -> 90,
      (98 to 99) -> 100,
      (100 to 100) -> 110)
    case MorphsMN.neotenic.name => RollConstant(10)
    case _ => {
      model.morphType match {
        case MorphType.Infomorph  => RollConstant(None)
        case MorphType.Synthmorph => RollConstant(None)
        case MorphType.Biomorph | MorphType.Pod => RollTable(
          (1 to 5) -> 10,
          (6 to 80) -> 20,
          (81 to 95) -> 30,
          (96 to 100) -> 40)
      }
    }
  }

  lazy val genderData: RollTableLike[String] = model.name match {
    case MorphsDF.fury.name => RollConstant("Female")
    case MorphsOR.pleasurePod.name => RollSubTables(RollTable(
      (1 to 50) -> RollConstant(genderId.toString()),
      (51 to 100) -> RollTable(
        (1 to 25) -> "Male",
        (26 to 50) -> "Female",
        (51 to 75) -> "Androgynous",
        (76 to 100) -> "Intersex")))
    case _ => {
      model.morphType match {
        case MorphType.Infomorph => RollTable(
          (1 to 80) -> genderId.toString,
          (81 to 100) -> "Genderless")
        case MorphType.Synthmorph => RollTable(
          (1 to 80) -> "Genderless",
          (81 to 90) -> "Male",
          (91 to 99) -> "Female",
          (100 to 100) -> "Intersex")
        case MorphType.Biomorph | MorphType.Pod => RollSubTables(RollTable(
          (1 to 50) -> RollConstant(genderId.toString()),
          (51 to 100) -> RollTable(
            (1 to 40) -> "Male",
            (41 to 80) -> "Female",
            (81 to 90) -> "Androgynous",
            (91 to 100) -> "Intersex")))
      }
    }
  }

  lazy val dataEyes: RollTableLike[String] = model.morphType match {
    case MorphType.Infomorph => RollSubTables(RollTable(
      (1 to 80) -> eyeColourData,
      (81 to 100) -> RollConstant("no visible")))
    case MorphType.Synthmorph => RollSubTables(RollTable(
      (1 to 30) -> eyeColourData,
      (31 to 80) -> RollConstant("camera lense"),
      (81 to 100) -> RollConstant("no visible")))
    case MorphType.Biomorph | MorphType.Pod => RollSubTables(RollTable(
      (1 to 90) -> eyeColourData,
      (91 to 100) -> RollConstant("cybernetic")))
  };

  lazy val dataHair: RollTableLike[Option[String]] = model.morphType match {
    case MorphType.Infomorph => RollTable(
      (1 to 30) -> None,
      (31 to 50) -> "short",
      (51 to 60) -> "medium length",
      (61 to 70) -> "long",
      (71 to 80) -> "afro",
      (81 to 85) -> "braided (dreads)",
      (86 to 90) -> "braided (cornrows)",
      (91 to 95) -> "mohawk",
      (96 to 100) -> "animated")
    case MorphType.Synthmorph => RollTable(
      (1 to 90) -> None,
      (91 to 91) -> "short",
      (92 to 92) -> "medium length",
      (93 to 93) -> "long",
      (94 to 94) -> "afro",
      (95 to 95) -> "braided (dreads)",
      (96 to 96) -> "braided (cornrows)",
      (97 to 97) -> "mohawk",
      (98 to 100) -> "tentacles")
    case MorphType.Biomorph | MorphType.Pod => RollTable(
      (1 to 10) -> None,
      (11 to 20) -> "balding",
      (21 to 50) -> "short",
      (51 to 60) -> "medium length",
      (61 to 70) -> "long",
      (71 to 80) -> "afro",
      (81 to 85) -> "braided (dreads)",
      (86 to 90) -> "braided (cornrows)",
      (91 to 95) -> "mohawk",
      (96 to 100) -> "covered")
  };

  lazy val dataSkin: RollTableLike[String] = model.morphType match {
    case MorphType.Infomorph => RollSubTables(RollTable(
      (1 to 30) -> RollConstant("translucent surface"),
      (31 to 90) -> skinColourData.map(c => s"$c skin"),
      (96 to 100) -> RollConstant("animated surface")))
    case MorphType.Synthmorph => RollTable(
      (1 to 60) -> "plain steel shell",
      (61 to 70) -> "steel shell with chrome highlights",
      (71 to 80) -> "full chrome shell",
      (81 to 85) -> "ornate golden shell",
      (86 to 90) -> "ornate bronze shell",
      (91 to 95) -> "rugged titanium shell",
      (96 to 100) -> "ornate platinum shell")
    case MorphType.Biomorph | MorphType.Pod => RollSubTables(RollTable(
      (1 to 70) -> skinColourData.map(c => s"$c skin"),
      (71 to 85) -> skinColourData.map(c => s"$c visibly tattooed skin"),
      (86 to 90) -> skinColourData.map(c => s"$c heavily tattooed skin"),
      (91 to 95) -> RollConstant("completely painted skin"),
      (96 to 100) -> RollConstant("completely covered skin")))
  };

  override def label: String = "Morph Instance";
  override def source: String = "Lars Kroll";
  override def roll(rand: Random): Result = {
    val visibleAge = ageData.randomElement(rand).flatMap(_.flatMap(v => (Dice.`1d10` + v).randomElement(rand)));
    val visibleGender = genderData.randomElement(rand).get;

    val eyes = s"${dataEyes.randomElement(rand).get} eyes";
    val hairO = for {
      hO <- dataHair.randomElement(rand);
      h <- hO;
      c <- hairColourData.randomElement(rand)
    } yield s"$h $c hair";
    val hair = hairO.getOrElse("no hair");
    val skin = dataSkin.randomElement(rand).get;
    val appearance = s"The morph has $skin, $hair, and $eyes.";

    val aptBoni = model.aptitudeBonus; // TODO apply choices
    MorphInstance(
      label = "Random Instance",
      model = model.name,
      morphType = model.morphType,
      descr = s"""${model.descr}
+++
$appearance""",
      visibleGender = Some(visibleGender),
      visibleAge = visibleAge,
      location = Some("ACTIVE"),
      enhancements = model.enhancements,
      traits = model.traits,
      movement = model.movement,
      aptitudeMax = model.aptitudeMax,
      aptitudeBonus = aptBoni,
      otherEffects = model.otherEffects,
      attacks = model.attacks,
      durability = model.durability,
      armour = model.armour)
  }
}

object MorphInstantiation {
  def forModel(morph: MorphModel, genderIdentity: GenderIdentity.GenderIdentity): MorphInstantiation = new MorphInstantiation(morph, genderIdentity);

  val eyeColourData: RollTableLike[String] = RollTable(
    (1 to 10) -> "amber",
    (11 to 20) -> "blue",
    (21 to 50) -> "brown",
    (51 to 60) -> "gray",
    (61 to 70) -> "green",
    (71 to 80) -> "hazel",
    (81 to 85) -> "red",
    (86 to 90) -> "violet",
    (91 to 95) -> "black",
    (96 to 100) -> "white");

  val hairColourData: RollTableLike[String] = RollTable(
    (1 to 20) -> "blond",
    (21 to 50) -> "brown",
    (51 to 60) -> "black",
    (61 to 70) -> "red",
    (71 to 80) -> "gray",
    (81 to 90) -> "white",
    (91 to 93) -> "blue",
    (94 to 96) -> "green",
    (97 to 98) -> "purple",
    (99 to 100) -> "pink");

  val skinColourData: Array[String] = Array("pale", "fair", "white", "light brown", "olive", "brown", "dark brown", "black");
}

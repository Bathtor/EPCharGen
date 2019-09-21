package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._

import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success, Try}

import Implicits.{RandomArray, int2opt, str2opt};

class MorphInstantiation(val model: MorphModel, val genderId: GenderIdentity, val birthMorph: Boolean, val age: Int)
    extends Table
    with StrictLogging {
  import MorphInstantiation._;
  import Implicits.constToRollTable;

  override type Result = MorphInstance;

  val ageData: RollTableLike[Option[Int]] = if (!birthMorph) {
    model.name match {
      case MorphsDF.flat.name | MorphsS.splicer.name =>
        RollTable((1 to 25) -> 30,
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
          case MorphType.Biomorph | MorphType.Pod =>
            RollTable((1 to 5) -> 10, (6 to 80) -> 20, (81 to 95) -> 30, (96 to 100) -> 40)
        }
      }
    }
  } else {
    require(age > 0);
    Some(age)
  }

  lazy val genderData: RollTableLike[String] = {
    val table: RollTableLike[String] = model.name match {
      case MorphsDF.fury.name => RollConstant("Female")
      case MorphsOR.pleasurePod.name =>
        RollSubTables(
          RollTable((1 to 50) -> RollConstant(genderId.toString()),
                    (51 to 100) -> RollTable((1 to 25) -> "Male",
                                             (26 to 50) -> "Female",
                                             (51 to 75) -> "Androgynous",
                                             (76 to 100) -> "Intersex"))
        )
      case _ => {
        model.morphType match {
          case MorphType.Infomorph => RollTable((1 to 80) -> genderId.toString, (81 to 100) -> "Genderless")
          case MorphType.Synthmorph =>
            RollTable((1 to 80) -> "Genderless",
                      (81 to 90) -> "Male",
                      (91 to 99) -> "Female",
                      (100 to 100) -> "Intersex")
          case MorphType.Biomorph | MorphType.Pod =>
            RollSubTables(
              RollTable((1 to 50) -> RollConstant(genderId.toString()),
                        (51 to 100) -> RollTable((1 to 40) -> "Male",
                                                 (41 to 80) -> "Female",
                                                 (81 to 90) -> "Androgynous",
                                                 (91 to 100) -> "Intersex"))
            )
        }
      }
    };
    if (birthMorph) {
      RollSubTables(RollTable((1 to 199) -> genderId.toString(), (200 to 200) -> table))
    } else {
      table
    }
  }

  lazy val dataEyes: RollTableLike[String] = model.morphType match {
    case MorphType.Infomorph =>
      RollSubTables(RollTable((1 to 80) -> eyeColourData, (81 to 100) -> RollConstant("no visible")))
    case MorphType.Synthmorph =>
      RollSubTables(
        RollTable((1 to 30) -> eyeColourData,
                  (31 to 80) -> RollConstant("camera lense"),
                  (81 to 100) -> RollConstant("no visible"))
      )
    case MorphType.Biomorph | MorphType.Pod =>
      RollSubTables(RollTable((1 to 90) -> eyeColourData, (91 to 100) -> RollConstant("cybernetic")))
  };

  lazy val dataHair: RollTableLike[Option[String]] = model.morphType match {
    case MorphType.Infomorph =>
      RollTable(
        (1 to 30) -> None,
        (31 to 50) -> "short",
        (51 to 60) -> "medium length",
        (61 to 70) -> "long",
        (71 to 80) -> "afro",
        (81 to 85) -> "braided (dreads)",
        (86 to 90) -> "braided (cornrows)",
        (91 to 95) -> "mohawk",
        (96 to 100) -> "animated"
      )
    case MorphType.Synthmorph =>
      RollTable(
        (1 to 90) -> None,
        (91 to 91) -> "short",
        (92 to 92) -> "medium length",
        (93 to 93) -> "long",
        (94 to 94) -> "afro",
        (95 to 95) -> "braided (dreads)",
        (96 to 96) -> "braided (cornrows)",
        (97 to 97) -> "mohawk",
        (98 to 100) -> "tentacles"
      )
    case MorphType.Biomorph | MorphType.Pod =>
      RollTable(
        (1 to 10) -> None,
        (11 to 20) -> "balding",
        (21 to 50) -> "short",
        (51 to 60) -> "medium length",
        (61 to 70) -> "long",
        (71 to 80) -> "afro",
        (81 to 85) -> "braided (dreads)",
        (86 to 90) -> "braided (cornrows)",
        (91 to 95) -> "mohawk",
        (96 to 100) -> "covered"
      )
  };

  lazy val dataSkin: RollTableLike[String] = model.morphType match {
    case MorphType.Infomorph =>
      RollSubTables(
        RollTable((1 to 30) -> RollConstant("translucent surface"),
                  (31 to 90) -> skinColourData.map(c => s"$c skin"),
                  (96 to 100) -> RollConstant("animated surface"))
      )
    case MorphType.Synthmorph =>
      RollTable(
        (1 to 60) -> "plain steel shell",
        (61 to 70) -> "steel shell with chrome highlights",
        (71 to 80) -> "full chrome shell",
        (81 to 85) -> "ornate golden shell",
        (86 to 90) -> "ornate bronze shell",
        (91 to 95) -> "rugged titanium shell",
        (96 to 100) -> "ornate platinum shell"
      )
    case MorphType.Biomorph | MorphType.Pod =>
      RollSubTables(
        RollTable(
          (1 to 70) -> skinColourData.map(c => s"$c skin"),
          (71 to 85) -> skinColourData.map(c => s"$c visibly tattooed skin"),
          (86 to 90) -> skinColourData.map(c => s"$c heavily tattooed skin"),
          (91 to 95) -> RollConstant("completely painted skin"),
          (96 to 100) -> RollConstant("completely covered skin")
        )
      )
  };

  override def label: String = "Morph Instance";
  override def source: String = "Lars Kroll";
  override def roll(rand: Random): Result = {
    val visibleAge: Option[Int] = for {
      decadeO <- ageData.randomElement(rand);
      _ <- Some(println(s"Got decade: $decadeO"));
      decade <- decadeO;
      ageRoll <- Some(Dice.`1d10` + decade);
      age <- ageRoll.randomElement(rand);
      _ <- Some(println(s"Got age: $age"))
    } yield age;
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

    val rawAptBoni = model.aptitudeBonus;
    val aptBoni = makeChoices(rawAptBoni, rand);

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
      armour = model.armour
    )
  }

  private def makeChoices(rawAptBoni: AptitudeValues, rand: Random): AptitudeValues = {
    model.playerDecisions match {
      case Some(c) => {
        val choicesT = ChoiceParser.parseString(c);
        choicesT match {
          case Success(choices) => {
            val pool =
              rawAptBoni.labelledValues.filter(_._2.getOrElse(0) == 0).map(t => Aptitude.withName(t._1)).toBuffer;
            var aptBoni = rawAptBoni;
            choices.foreach { c =>
              for (_ <- 1 to c.repetition) {
                if (pool.isEmpty) { // can't fulfil "other" so just use an existing one
                  val sorted = rawAptBoni.labelledValues.map(t => (t._2.getOrElse(0) -> t._1)).sortBy(_._1);
                  val min = sorted.head;
                  aptBoni = updateAptVByName(aptBoni, min._2, current => current + c.bonus);
                } else { // pick from pool
                  val pos = rand.nextInt(pool.size);
                  val item = pool.remove(pos);
                  aptBoni = updateAptVByApt(aptBoni, item, current => c.bonus);
                }
              }
            }
            aptBoni
          }
          case Failure(e) => {
            logger.error(s"Could not parse player decision: $c", e);
            throw e;
          }
        }
      }
      case None => rawAptBoni
    }
  }

  private def updateAptVByName(av: AptitudeValues, apt: String, updateValue: Int => Int): AptitudeValues =
    updateAptVByApt(av, Aptitude.withName(apt), updateValue);
  private def updateAptVByApt(av: AptitudeValues, apt: Aptitude, updateValue: Int => Int): AptitudeValues = {
    apt match {
      case Aptitude.COG => av.copy(cog = updateValue(av.cog.getOrElse(0)));
      case Aptitude.COO => av.copy(coo = updateValue(av.coo.getOrElse(0)));
      case Aptitude.INT => av.copy(int = updateValue(av.int.getOrElse(0)));
      case Aptitude.REF => av.copy(ref = updateValue(av.ref.getOrElse(0)));
      case Aptitude.SAV => av.copy(sav = updateValue(av.sav.getOrElse(0)));
      case Aptitude.SOM => av.copy(som = updateValue(av.som.getOrElse(0)));
      case Aptitude.WIL => av.copy(cog = updateValue(av.wil.getOrElse(0)));
    }
  }
}

object ChoiceParser {
  import fastparse._, SingleLineWhitespace._;

  case class Choice(repetition: Int, bonus: Int, exception: Option[Aptitude])

  val aptNames: Seq[String] = Aptitude.values.map(_.toString());

  def parseString(s: String): Try[Seq[Choice]] = {
    Try {
      val res = parse(s, aptitudeChoiceMatcher(_));
      res match {
        case Parsed.Success(c, _) => c
        case f: Parsed.Failure => {
          val msg = f.trace().longMsg;
          throw new RuntimeException(msg);
        }
      }
    }
  }

  private def aptitudeChoiceMatcher[_: P]: P[Seq[Choice]] = P(singleChoice.rep(sep = ","));
  private def singleChoice[_: P]: P[Choice] = P(mod ~/ "to" ~/ rep ~/ "other" ~/ apt ~/ cond.?).map {
    case (bonus, repetition, exception) => Choice(repetition, bonus, exception)
  };
  private def cond[_: P]: P[Aptitude] =
    P("except" ~/ StringIn("COG", "COO", "INT", "REF", "SAV", "SOM", "WIL").!).map(s => Aptitude.withName(s));
  private def apt[_: P]: P[Unit] = P(StringIn("aptitude", "aptitudes", "aptitude of the player’s choice"));
  private def rep[_: P]: P[Int] =
    P(StringIn("one", "two", "three").!).map(
      s =>
        s match {
          case "one"   => 1
          case "two"   => 2
          case "three" => 3
        }
    );
  private def mod[_: P]: P[Int] = P(posMod | negMod);
  private def posMod[_: P]: P[Int] = P("+" ~/ int);
  private def negMod[_: P]: P[Int] = P("-" ~/ int).map(i => -i);
  private def int[_: P]: P[Int] = P(CharsWhileIn("0123456789").!).map(s => s.toInt);
}

object MorphInstantiation {

  def forModel(morph: MorphModel, genderIdentity: GenderIdentity): MorphInstantiation =
    new MorphInstantiation(morph, genderIdentity, false, -1);
  def birthMorph(morph: MorphModel, genderIdentity: GenderIdentity, age: Int): MorphInstantiation = {
    require(age > 0);
    new MorphInstantiation(morph, genderIdentity, true, age);
  }

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
    (96 to 100) -> "white"
  );

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
    (99 to 100) -> "pink"
  );

  val skinColourData: Array[String] =
    Array("pale", "fair", "white", "light brown", "olive", "brown", "dark brown", "black");
}

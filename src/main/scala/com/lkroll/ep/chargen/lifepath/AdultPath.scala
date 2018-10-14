package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._

case class AdultPathIndex(i: Int) extends PathIndex {
  override def tableIndex = i;
}
case class AdultPath(kind: String, path: PathGroup, pkg: Either[ThreePackageGroup[FocusPackage], CustomizationPackage])
case class PathGroup(index: Int, label: String, faction: FactionPathIndex)

class AdultPathTable(
  val nextPath:   AdultPathIndex,
  val allowAsync: Boolean,
  val isAsync:    Boolean) extends Table {
  import AdultPathTable._;
  import Implicits.RandomArray;

  override type Result = AdultPath;

  override def label: String = "Adult Paths";
  override def source: String = "Transhuman p.60-61";
  override def roll(rand: Random): Result = {
    val pathChoice = data0.randomElement(rand).get;
    val nextIndex = pathChoice match {
      case PathChoice.MostRecentBackground => nextPath.i match {
        case 1 => data1.randomElement(rand).get
        case i => i
      }
      case PathChoice.Specialize => 12
      case PathChoice.Switch     => data1.randomElement(rand).get
    };
    packageFromIndex(rand, nextIndex, pathChoice.kind)
  }

  private[lifepath] def packageFromIndex(rand: Random, nextIndex: Int, pckind: String): Result = {
    nextIndex match {
      case 2 => AdultPath(
        kind = pckind,
        path = PathGroup(2, "Autonomist", FactionPathIndex(4)),
        pkg = Left(data2.randomElement(rand).get))
      case 3 => AdultPath(
        kind = pckind,
        path = PathGroup(3, "Civilian", FactionPathIndex(5)),
        pkg = Left(data3.randomElement(rand).get))
      case 4 => AdultPath(
        kind = pckind,
        path = PathGroup(4, "Criminal", FactionPathIndex(6)),
        pkg = Left(data4.randomElement(rand).get))
      case 5 => AdultPath(
        kind = pckind,
        path = PathGroup(5, "Elite", FactionPathIndex(7)),
        pkg = Left(data5.randomElement(rand).get))
      case 6 => AdultPath(
        kind = pckind,
        path = PathGroup(6, "Enclaver", FactionPathIndex(8)),
        pkg = Left(data6.randomElement(rand).get))
      case 7 => AdultPath(
        kind = pckind,
        path = PathGroup(7, "Indenture", FactionPathIndex(9)),
        pkg = Left(data7.randomElement(rand).get))
      case 8 => AdultPath(
        kind = pckind,
        path = PathGroup(8, "Military", FactionPathIndex(10)),
        pkg = Left(data8.randomElement(rand).get))
      case 9 => AdultPath(
        kind = pckind,
        path = PathGroup(9, "Scientist", FactionPathIndex(11)),
        pkg = Left(data9.randomElement(rand).get))
      case 10 => AdultPath(
        kind = pckind,
        path = PathGroup(10, "Spacer", FactionPathIndex(12)),
        pkg = Left(data10.randomElement(rand).get))
      case 11 => AdultPath(
        kind = pckind,
        path = PathGroup(11, "Techie", FactionPathIndex(13)),
        pkg = Left(data11.randomElement(rand).get))
      case 12 => {
        var pkg: Option[CustomizationPackage] = None;
        while (pkg.isEmpty) {
          pkg = data12.randomElement(rand);
          val label = pkg.get.label;
          if (!allowAsync && (label.equalsIgnoreCase("Async") || label.equalsIgnoreCase("Async Adept"))) {
            pkg = None; // reroll
          } else if (!isAsync && label.equalsIgnoreCase("Mentalist")) {
            pkg = None; // reroll
          }
        }
        AdultPath(
          kind = pckind,
          path = PathGroup(12, "Customization", FactionPathIndex(-1)),
          pkg = Right(pkg.get))
      }
      case _ => ???
    }
  }
}

object AdultPathTable {
  import FocusPackages._;
  import CustomizationPackages._;
  import Implicits.RandomArray

  def preFall(nextPath: AdultPathIndex): AdultPathTable = new AdultPathTable(nextPath, false, false);

  def postFall(nextPath: AdultPathIndex, isAsync: Boolean): AdultPathTable = new AdultPathTable(nextPath, true, isAsync);

  sealed trait PathChoice {
    def kind: String;
  }
  object PathChoice {
    case object MostRecentBackground extends PathChoice {
      override def kind: String = "Stay on the path.";
    }
    case object Specialize extends PathChoice {
      override def kind: String = "Specialize.";
    }
    case object Switch extends PathChoice {
      override def kind: String = "Switch gears.";
    }
  }

  private val data0: RollTable[PathChoice] = RollTable(
    (1 to 3) -> PathChoice.MostRecentBackground,
    (4 to 6) -> PathChoice.Specialize,
    (7 to 10) -> PathChoice.Switch);

  private[lifepath] val data1: RandomArray[Int] = Array(2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  private val data2: RandomArray[ThreePackageGroup[FocusPackage]] = Array(
    academic,
    activist,
    botJammer,
    covertOps,
    explorer,
    genehacker,
    hacker,
    medic,
    scientist,
    techie);
  private val data3: RandomArray[ThreePackageGroup[FocusPackage]] = Array(
    activist,
    conArtist,
    dealer,
    face,
    investigator,
    journo,
    smartAnimalHandler,
    soldier,
    techie,
    thief);
  private val data4: RandomArray[ThreePackageGroup[FocusPackage]] = Array(
    assassin,
    conArtist,
    covertOps,
    dealer,
    egoHunter,
    enforcer,
    hacker,
    pirate,
    smuggler,
    thief);
  private val data5: RollTable[ThreePackageGroup[FocusPackage]] = RollTable(
    (1 to 1) -> academic,
    (2 to 2) -> dealer,
    (3 to 4) -> face,
    (5 to 6) -> icon,
    (7 to 7) -> journo,
    (8 to 8) -> medic,
    (9 to 9) -> psychosurgeon,
    (10 to 10) -> scientist);
  private val data6: RollTable[ThreePackageGroup[FocusPackage]] = RollTable(
    (1 to 1) -> academic,
    (2 to 2) -> conArtist,
    (3 to 4) -> dealer,
    (5 to 5) -> face,
    (6 to 6) -> icon,
    (7 to 7) -> investigator,
    (8 to 8) -> journo,
    (9 to 9) -> medic,
    (10 to 10) -> psychosurgeon);
  private val data7: RandomArray[ThreePackageGroup[FocusPackage]] = Array(
    activist,
    bodyguard,
    botJammer,
    conArtist,
    enforcer,
    pirate,
    scavenger,
    smartAnimalHandler,
    smuggler,
    thief);
  private val data8: RollTable[ThreePackageGroup[FocusPackage]] = RollTable(
    (1 to 1) -> assassin,
    (2 to 2) -> bodyguard,
    (3 to 3) -> covertOps,
    (4 to 4) -> egoHunter,
    (5 to 5) -> enforcer,
    (6 to 6) -> investigator,
    (7 to 9) -> soldier,
    (10 to 10) -> spy);
  private val data9: RandomArray[ThreePackageGroup[FocusPackage]] = Array(
    academic,
    explorer,
    genehacker,
    investigator,
    medic,
    psychosurgeon,
    scientist,
    scientist,
    smartAnimalHandler,
    techie);
  private val data10: RollTable[ThreePackageGroup[FocusPackage]] = RollTable(
    (1 to 1) -> botJammer,
    (2 to 2) -> egoHunter,
    (3 to 4) -> explorer,
    (5 to 5) -> pirate,
    (6 to 6) -> scavenger,
    (7 to 7) -> soldier,
    (8 to 9) -> smuggler,
    (10 to 10) -> spy);
  private val data11: RollTable[ThreePackageGroup[FocusPackage]] = RollTable(
    (1 to 1) -> botJammer,
    (2 to 2) -> explorer,
    (3 to 3) -> genehacker,
    (4 to 5) -> hacker,
    (6 to 6) -> scavenger,
    (7 to 7) -> scientist,
    (8 to 8) -> spy,
    (9 to 10) -> techie);

  private val data12: RollTable[CustomizationPackage] = RollTable(
    (1 to 4) -> artist,
    (5 to 8) -> async,
    (9 to 12) -> asyncAdept,
    (13 to 16) -> athletics,
    (17 to 20) -> computerTraining,
    (21 to 24) -> connected,
    (25 to 28) -> gearhead,
    (29 to 32) -> heavyWeaponsTraining,
    (33 to 39) -> jackOfAllTrades,
    (40 to 46) -> lucky,
    (47 to 50) -> martialArtsTraining,
    (51 to 54) -> mentalist,
    (55 to 61) -> networker,
    (62 to 65) -> paramedic,
    (66 to 69) -> slacker,
    (70 to 73) -> sneaker,
    (74 to 77) -> socialButterfly,
    (78 to 81) -> spacer,
    (82 to 85) -> student,
    (86 to 89) -> survivalTraining,
    (90 to 93) -> techTraining,
    (94 to 100) -> weaponsTraining);
}

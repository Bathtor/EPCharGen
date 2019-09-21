package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen.utils._

case class Focus(label: String, pkg: FocusPackage)

class FocusTable(val archetype: Archetype, val factionLevel: PackageLevel) extends Table {
  import Implicits.constToRollTable;

  override type Result = Focus;

  override def label: String = "Archetype Focus";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    val res = data.randomElement(rand).get;
    val pkg = res.ofLevel(factionLevel).get;
    val label = res.label;
    Focus(label, pkg)
  }

  private lazy val data: RollTable[ThreePackageGroup[FocusPackage]] = {
    import FocusPackages._;

    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 5) -> activist,
          (6 to 15) -> conArtist,
          (16 to 20) -> dealer,
          (21 to 40) -> face,
          (41 to 60) -> icon,
          (61 to 65) -> investigator,
          (66 to 75) -> journo,
          (76 to 78) -> scannerAsync,
          (79 to 85) -> smuggler,
          (86 to 100) -> spy
        )
      case Archetype.Fighter =>
        RollTable(
          (1 to 5) -> activist,
          (6 to 15) -> assassin,
          (16 to 25) -> bodyguard,
          (26 to 27) -> combatAsync,
          (28 to 35) -> covertOps,
          (36 to 40) -> egoHunter,
          (41 to 55) -> enforcer,
          (56 to 58) -> explorer,
          (59 to 63) -> pirate,
          (64 to 68) -> scavenger,
          (69 to 72) -> smuggler,
          (73 to 88) -> soldier,
          (89 to 89) -> spy,
          (90 to 100) -> wrecker
        )
      case Archetype.Hacker =>
        RollTable(
          (1 to 5) -> academic,
          (6 to 14) -> activist,
          (15 to 20) -> botJammer,
          (21 to 30) -> covertOps,
          (31 to 50) -> hacker,
          (51 to 55) -> investigator,
          (56 to 60) -> journo,
          (61 to 65) -> scavenger,
          (66 to 70) -> scientist,
          (71 to 80) -> spy,
          (81 to 95) -> techie,
          (96 to 100) -> thief
        )
      case Archetype.Scientist =>
        RollTable(
          (1 to 20) -> academic,
          (21 to 30) -> explorer,
          (31 to 40) -> genehacker,
          (41 to 50) -> medic,
          (51 to 55) -> psychosurgeon,
          (56 to 60) -> savantAsync,
          (61 to 90) -> scientist,
          (91 to 95) -> spy,
          (96 to 100) -> techie
        )
    }
  }
}

package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{CharImplicits, Skill, Skills}
import com.lkroll.ep.compendium.SkillDef
import com.lkroll.ep.compendium.data.DefaultSkills
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._

import scala.language.implicitConversions

case class ExtraPackage(prefix: String, pkg: PPPackage)

class ExtraPackageTable(val archetype: Archetype, val skills: List[Skill], val isAsync: Boolean) extends Table {
  import Implicits._;

  override type Result = ExtraPackage;

  implicit def customization2extra(pkg: CustomizationPackage): ExtraPackage = ExtraPackage(pkg.label, pkg);

  sealed trait PackageChoice;
  object PackageChoice {
    case object Focus extends PackageChoice;
    case object Faction extends PackageChoice;
    case object Customization extends PackageChoice;
  }

  private val data: RollTable[PackageChoice] = RollTable((1 to 4) -> PackageChoice.Focus,
                                                         (5 to 6) -> PackageChoice.Faction,
                                                         (7 to 10) -> PackageChoice.Customization);

  override def label: String = "Extra Packages";
  override def source: String = "Homebew";

  override def roll(rand: Random): Result = {
    val res = ensureArchetypeSkills(rand);
    res match {
      case Some(pkg) => pkg
      case None      => pickAnyPackage(rand)
    }
  }

  private def ensureArchetypeSkills(rand: Random): Option[Result] = {
    import DefaultSkills._;

    archetype match {
      case Archetype.Butterfly => {
        if (!hasAnySkillAbove(30, deception, persuasion)) {
          Some(CustomizationPackages.socialButterfly)
        } else {
          None
        }
      }
      case Archetype.Fighter => {
        if (!hasAnySkillAbove(30, fray)) {
          Some(CustomizationPackages.essentialSkills)
        } else if (!hasAnySkillAbove(30,
                                     blades,
                                     beamWeapons,
                                     kineticWeapons,
                                     seekerWeapons,
                                     sprayWeapons,
                                     throwingWeapons,
                                     unarmedCombat)) {
          val table: RollTableLike[CustomizationPackage] = Array(
            CustomizationPackages.heavyWeaponsTraining,
            CustomizationPackages.survivalTraining,
            CustomizationPackages.weaponsTraining,
            CustomizationPackages.martialArtsTraining
          );
          val res = table.randomElement(rand).get;
          Some(res)
        } else {
          None
        }
      }
      case Archetype.Hacker => {
        if (!hasAnySkillAbove(30, infosec)) {
          Some(CustomizationPackages.computerTraining)
        } else if (!hasAnySkillAbove(30, programming, interfacing)) {
          val table: RollTableLike[CustomizationPackage] =
            Array(CustomizationPackages.computerTraining, CustomizationPackages.techTraining);
          val res = table.randomElement(rand).get;
          Some(res)
        } else {
          None
        }
      }
      case Archetype.Scientist => {
        if (!hasAnySkillAbove(30, research)) {
          Some(CustomizationPackages.student)
        } else if (!hasAnySkillAbove(30, academics)) {
          Some(CustomizationPackages.student)
        } else {
          None
        }
      }
    }
  }

  private def hasAnySkillAbove(minRank: Int, targets: SkillDef*): Boolean = {
    skills.foreach { s =>
      if (targets.exists(t => t.name == s.name)) {
        if (s.ranks >= minRank) {
          return true;
        }
      }
    }
    return false;
  }

  private def pickAnyPackage(rand: Random): Result = {
    data.randomElement(rand).get match {
      case PackageChoice.Focus => {
        val path = new FocusTable(archetype, PackageLevel.Basic).roll(rand);
        val label = path.label.replaceAll(" 1PP", "");
        ExtraPackage(label, path.pkg)
      }
      case PackageChoice.Faction => {
        val path = new FactionTable(archetype, PackageLevel.Basic).roll(rand);
        val label = path.label.replaceAll(" 1PP", "");
        ExtraPackage(label, path.pkg)
      }
      case PackageChoice.Customization => {
        customizationData.randomElement(rand).get
      }
    }
  }

  lazy val customizationData: RollTable[CustomizationPackage] = {
    import CustomizationPackages._;

    archetype match {
      case Archetype.Butterfly if isAsync =>
        RollTable(
          (1 to 6) -> artist,
          (7 to 12) -> essentialSkills,
          (13 to 13) -> mentalist,
          (14 to 16) -> spacer,
          (17 to 30) -> networker,
          (31 to 36) -> student,
          (37 to 37) -> asyncAdept,
          (38 to 40) -> paramedic,
          (41 to 45) -> survivalTraining,
          (46 to 48) -> athletics,
          (49 to 55) -> jackOfAllTrades,
          (56 to 60) -> slacker,
          (61 to 65) -> lucky,
          (66 to 70) -> sneaker,
          (71 to 85) -> connected,
          (86 to 100) -> socialButterfly
        )
      case Archetype.Butterfly =>
        RollTable(
          (1 to 6) -> artist,
          (7 to 13) -> essentialSkills,
          (14 to 16) -> spacer,
          (17 to 30) -> networker,
          (31 to 37) -> student,
          (38 to 40) -> paramedic,
          (41 to 45) -> survivalTraining,
          (46 to 48) -> athletics,
          (49 to 55) -> jackOfAllTrades,
          (56 to 60) -> slacker,
          (61 to 65) -> lucky,
          (66 to 70) -> sneaker,
          (71 to 85) -> connected,
          (86 to 100) -> socialButterfly
        )
      case Archetype.Fighter if isAsync =>
        RollTable(
          (1 to 10) -> essentialSkills,
          (11 to 11) -> mentalist,
          (12 to 15) -> spacer,
          (16 to 16) -> asyncAdept,
          (17 to 26) -> heavyWeaponsTraining,
          (27 to 32) -> paramedic,
          (33 to 38) -> survivalTraining,
          (39 to 45) -> athletics,
          (46 to 50) -> jackOfAllTrades,
          (51 to 55) -> slacker,
          (56 to 60) -> lucky,
          (61 to 70) -> sneaker,
          (71 to 80) -> weaponsTraining,
          (81 to 100) -> martialArtsTraining
        )
      case Archetype.Fighter =>
        RollTable(
          (1 to 10) -> essentialSkills,
          (11 to 16) -> spacer,
          (17 to 26) -> heavyWeaponsTraining,
          (27 to 32) -> paramedic,
          (33 to 38) -> survivalTraining,
          (39 to 45) -> athletics,
          (46 to 50) -> jackOfAllTrades,
          (51 to 55) -> slacker,
          (56 to 60) -> lucky,
          (61 to 70) -> sneaker,
          (71 to 80) -> weaponsTraining,
          (81 to 100) -> martialArtsTraining
        )
      case Archetype.Hacker if isAsync =>
        RollTable(
          (1 to 10) -> essentialSkills,
          (11 to 11) -> mentalist,
          (12 to 19) -> spacer,
          (20 to 30) -> gearhead,
          (31 to 40) -> student,
          (41 to 46) -> survivalTraining,
          (47 to 55) -> jackOfAllTrades,
          (56 to 60) -> slacker,
          (61 to 70) -> techTraining,
          (71 to 80) -> computerTraining,
          (81 to 90) -> lucky,
          (91 to 100) -> sneaker
        )
      case Archetype.Hacker =>
        RollTable(
          (1 to 11) -> essentialSkills,
          (12 to 19) -> spacer,
          (20 to 30) -> gearhead,
          (31 to 40) -> student,
          (41 to 46) -> survivalTraining,
          (47 to 55) -> jackOfAllTrades,
          (56 to 60) -> slacker,
          (61 to 70) -> techTraining,
          (71 to 80) -> computerTraining,
          (81 to 90) -> lucky,
          (91 to 100) -> sneaker
        )
      case Archetype.Scientist if isAsync =>
        RollTable(
          (1 to 5) -> artist,
          (6 to 12) -> essentialSkills,
          (13 to 13) -> mentalist,
          (14 to 20) -> spacer,
          (21 to 30) -> gearhead,
          (31 to 40) -> student,
          (41 to 41) -> asyncAdept,
          (42 to 50) -> paramedic,
          (51 to 55) -> survivalTraining,
          (56 to 60) -> athletics,
          (61 to 65) -> jackOfAllTrades,
          (66 to 70) -> slacker,
          (71 to 80) -> techTraining,
          (81 to 90) -> computerTraining,
          (91 to 100) -> lucky
        )
      case Archetype.Scientist =>
        RollTable(
          (1 to 5) -> artist,
          (6 to 13) -> essentialSkills,
          (14 to 20) -> spacer,
          (21 to 30) -> gearhead,
          (31 to 40) -> student,
          (41 to 50) -> paramedic,
          (51 to 55) -> survivalTraining,
          (56 to 60) -> athletics,
          (61 to 65) -> jackOfAllTrades,
          (66 to 70) -> slacker,
          (71 to 80) -> techTraining,
          (81 to 90) -> computerTraining,
          (91 to 100) -> lucky
        )
    }
  }
}

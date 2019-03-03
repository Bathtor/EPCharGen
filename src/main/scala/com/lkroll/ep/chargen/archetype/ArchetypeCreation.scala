package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{ CharImplicits, Skills, MorphInstantiation, RepNetworks, GenderTables }
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitudes, GenderIdentity, MorphModel, MorphInstance, RepNetwork }
import com.lkroll.ep.compendium.data.DefaultSkills

import scala.language.postfixOps

sealed trait Archetype;
object Archetype {
  case object Fighter extends Archetype;
  case object Scientist extends Archetype;
  case object Hacker extends Archetype;
  case object Butterfly extends Archetype;

  val list: List[Archetype] = List(Fighter, Scientist, Hacker, Butterfly);

  def fromString(s: String): Archetype = s.toLowerCase() match {
    case "fighter"   => Fighter
    case "scientist" => Scientist
    case "hacker"    => Hacker
    case "butterfly" => Butterfly
  };
}

class ArchetypeCreation(
  val archetype: Archetype,
  val fair:      Boolean        = true,
  val firewall:  FirewallOption = FirewallOption.Skip,
  val gear:      Boolean        = false,
  val moxie:     Boolean        = true) extends CreationSystem {

  import Implicits.RandomArray;
  import CharImplicits.skilldef2skill;

  override def label: String = "Archetype";
  override def randomCharacter(rand: Random): CreationResult = {
    var stages: List[StageResult] = List(SystemResult(this));
    var history: List[String] = Nil;
    var skills: List[character.Skill] = Nil;
    var allPackages: List[PPPackage] = Nil;

    val apts = new AptitudeTemplateTable(archetype).roll(rand);
    stages ::= Stages.AptitudeTable(apts);

    val nativeLang = lifepath.LanguageTable.roll(rand).copy(ranks = 70);
    stages ::= Stages.NativeLangTable(nativeLang);
    skills ::= nativeLang;

    val ppDistribution = PPDistributionTable.roll(rand);
    stages ::= Stages.PPDistributionTable(ppDistribution);

    val backgroundResult = new BackgroundTable(archetype, ppDistribution.background).roll(rand);
    val background = backgroundResult.pkg.label;
    val startingMorph = backgroundResult.startingMorph;
    stages ::= Stages.BackgroundTable(backgroundResult);
    history ::= s"${backgroundResult.label} (${backgroundResult.group})";
    allPackages ::= backgroundResult.pkg;

    val genderId = if (background.contains("Infolife")) {
      GenderTables.agiGenderTable.randomElement(rand).get
    } else {
      GenderTables.normalGenderTable.randomElement(rand).get
    };

    val instantiateMorph = (m: MorphModel) => MorphInstantiation.forModel(m, genderId).roll(rand);
    var currentMorph: MorphInstance = instantiateMorph(startingMorph);

    val startingAge = lifepath.StartingAge.roll(rand);
    stages ::= Stages.StartingAge(startingAge);
    val yearOfBirth: String = {
      if (startingAge.age <= 10) {
        s"${10 - startingAge.age}AF"
      } else {
        val ageAtFall = startingAge.age - 10;
        s"${ageAtFall}BF"
      }
    };

    history ::= s"Born/Created ${yearOfBirth} in a ${currentMorph.visibleGender.getOrElse("")} ${currentMorph.model}.";
    history ::= s"Brought up ${nativeLang.field.getOrElse("not")} speaking.";

    var char = character.CharGenCharacter(
      name = "Anonymous",
      gender = genderId,
      aptitudes = Aptitudes(base = apts.aptitudes, morphBoni = currentMorph.aptitudeBonus, morphMax = currentMorph.aptitudeMax),
      skills = skills,
      background = background,
      startingMorph = startingMorph,
      activeMorph = currentMorph,
      history = history.reverse);

    char = startingAge.mods.foldLeft(char.copy(age = startingAge.age)) { (acc, mod) =>
      mod.applyTo(acc)
    };

    char = backgroundResult.pkg.applyTo(char, rand);

    val factionResult = new FactionTable(archetype, ppDistribution.faction).roll(rand);
    stages ::= Stages.FactionTable(factionResult);
    history ::= s"Joined the ${factionResult.label} faction";
    char = factionResult.pkg.applyTo(char, rand);
    allPackages ::= factionResult.pkg;
    char = char.copy(faction = factionResult.label); // override label to have it read nicer

    val focusResult = new FocusTable(archetype, ppDistribution.focus).roll(rand);
    stages ::= Stages.FocusTable(focusResult);
    history ::= s"Learned the skills of ${if (focusResult.label.matches("""[aeiouAEIOU].*""")) { "an" } else { "a" }} ${focusResult.label}";
    char = focusResult.pkg.applyTo(char, rand);
    allPackages ::= focusResult.pkg;

    var extraPackages: List[ExtraPackage] = Nil;
    if (fair) {
      while (allPackages.map(_.ppCost).sum < 10) {
        val extraPackageTable = new ExtraPackageTable(archetype, char.skills, char.isAsync);
        val extraPackage: ExtraPackage = extraPackageTable.roll(rand);
        if (allPackages.find(p => p.label.startsWith(extraPackage.prefix)).isEmpty) {
          extraPackages ::= extraPackage;
          char = extraPackage.pkg.applyTo(char, rand);
          allPackages ::= extraPackage.pkg;
        }
      }
    } else {
      var picked = false;
      while (!picked) {
        val extraPackageTable = new ExtraPackageTable(archetype, char.skills, char.isAsync);
        val extraPackage: ExtraPackage = extraPackageTable.roll(rand);
        if (allPackages.find(p => p.label.startsWith(extraPackage.prefix)).isEmpty) {
          extraPackages ::= extraPackage;
          char = extraPackage.pkg.applyTo(char, rand);
          allPackages ::= extraPackage.pkg;
        }
      }
    }
    stages ::= Stages.FinalPackages(extraPackages, allPackages.map(_.ppCost).sum);

    val hasIRep = char.rep.getOrElse(RepNetworks.iRep, 0) > 0;
    val hasNetFirewall = char.skills.find(s => DefaultSkills.networking.name == s.name && s.field.get == "Firewall").isDefined;
    firewall match {
      case FirewallOption.Allow => {
        if (hasIRep || hasNetFirewall) {
          if (!hasIRep) {
            char = char.copy(rep = char.rep + (RepNetworks.iRep -> 5)); // Firewall chars should have at least 5 i-Rep
          }
        }
      }
      case FirewallOption.Always => {
        if (!hasIRep) {
          char = char.copy(rep = char.rep + (RepNetworks.iRep -> 5)); // Firewall chars should have at least 5 i-Rep
        }
      }
      case FirewallOption.Skip => {
        if (hasIRep) {
          val points = char.rep(RepNetworks.iRep);
          val picked = RepNetworks.list.filter(n => n != RepNetworks.iRep).toArray.randomElement(rand).get;
          val updated = char.rep.getOrElse(picked, 0) + points;
          var newRep = char.rep;
          newRep -= RepNetworks.iRep;
          newRep += (picked -> updated);
          char = char.copy(rep = newRep)
        }
        if (hasNetFirewall) {
          val (netFire, rest) = char.skills.partition(s => DefaultSkills.networking.name == s.name && s.field.get == "Firewall");
          val ranks = netFire.map(_.ranks).sum;
          val picked = DefaultSkills.networking.sampleFields.get.toArray.filterNot(_ == "Firewall").randomElement(rand).get;
          val skill = DefaultSkills.networking.withField(picked).instance(ranks);
          char = char.copy(skills = skill :: rest);
        }
      }
    }

    // Needs to happen before gear selection to make requirements work properly
    val combinationResult = character.CombineEverything(rand, char, moxie);
    char = combinationResult.char;
    stages ::= Stages.CombineEverything(combinationResult);

    val resleeve = new ResleeveTable(archetype, startingMorph, background, startingAge.age, char.isAsync).roll(rand);
    resleeve match {
      case Resleeve.NewMorph(m) => {
        currentMorph = instantiateMorph(m);
        history ::= s"Char had to resleeve into a ${currentMorph.visibleGender.getOrElse("")} ${currentMorph.model}.";
        char = char.copy(activeMorph = currentMorph);
      }
      case Resleeve.None => {
        history ::= "Char survived in eir original morph.";
      }
    }

    val startingCredit = lifepath.StartingCreditTable.roll(rand);
    if (gear) {
      var remainingCredit = startingCredit + char.startingCredit;
      var availablePacks = GearPackages.list.toArray.filter(p =>
        p.usableBy(char) && (p.creditCost <= remainingCredit));
      var gearPacks: List[GearPackage] = Nil;
      while (!availablePacks.isEmpty) {
        val pack = availablePacks.randomElement(rand).get;
        remainingCredit -= pack.creditCost;
        availablePacks = availablePacks.filter(p =>
          (p.label != pack.label) && (p.creditCost <= remainingCredit));
        gearPacks ::= pack;
      }
      remainingCredit = Math.max(0, remainingCredit);
      stages ::= Stages.Gear(Left((remainingCredit, gearPacks)));
      char = gearPacks.foldLeft(char.copy(startingCredit = remainingCredit)) { (acc, gp) =>
        gp.applyTo(acc, rand)
      };
    } else {
      stages ::= Stages.Gear(Right(startingCredit));
      char = char.copy(startingCredit = Math.max(0, char.startingCredit + startingCredit));
    }

    CreationResult(char.copy(history = history.reverse), stages.reverse)
  }

}

object Stages {
  import rendering.Renderer

  case class AptitudeTable(result: character.AptitudeTemplate) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 1: Aptitude Template");
      renderer.value(result.label);
      val aptvals = result.aptitudes.labelledValues.map {
        case (k, v) => (k.toUpperCase() -> v.get.toString)
      } toList;
      val aptpair = aptvals.unzip;
      renderer.table(List(aptpair._1, aptpair._2));
    }
  }

  case class NativeLangTable(result: character.Skill) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 2: Native Language");
      result.render(renderer);
    }
  }

  case class PPDistributionTable(result: PPDistribution) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 3: PP Distribution");
      result.render(renderer);
    }
  }

  case class BackgroundTable(result: Background) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 4: Background");
      renderer.section(result.label);
      renderer.note(result.group);
      renderer.newline();
      renderer.labelled("Background Package", result.pkg.label);
      renderer.raw(", ");
      renderer.labelled("Starting Morph", result.startingMorph.name);
    }
  }

  case class StartingAge(result: lifepath.StartingAgeResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 5: Starting Age");
      renderer.value(result.age.toString());
      if (result.skipPreFall) {
        renderer.note("Skipping Pre-Fall Paths")
      }
      renderer.newline();
      renderer.labelled("Effect", result.mods.toString()); // TODO ugly as fuck
    }
  }

  case class FactionTable(result: Faction) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 6: Faction");
      renderer.section(result.label);
      renderer.newline();
      renderer.labelled("Faction Package", result.pkg.label);
    }
  }

  case class FocusTable(result: Focus) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 7: Focus");
      renderer.section(result.label);
      renderer.newline();
      renderer.labelled("Focus Package", result.pkg.label);
    }
  }

  case class FinalPackages(result: List[ExtraPackage], ppTotal: Int) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 10: Final Packages");
      renderer.value(s"Added ${result.size} packages.");
      renderer.newline();
      renderer.labelled("Total PP", ppTotal.toString());
      renderer.section("Extra Packages");
      renderer.list(result.map(_.pkg.label));
    }
  }

  case class Gear(result: Either[(Int, List[GearPackage]), Int]) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 13: Gear");
      result match {
        case Left((startingCredit, gear)) => {
          renderer.labelled("Starting Credit", startingCredit.toString());
          renderer.list(gear.map(gp => s"${gp.label} (${gp.creditCost} credits)"))
        }
        case Right(startingCredit) => renderer.labelled("Starting Credit", startingCredit.toString())
      }
    }
  }

  case class CombineEverything(result: character.CombinationResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 14: Combine Everything");
      renderer.list(result.log);
    }
  }
}

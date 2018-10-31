package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{ CharImplicits, Skills, MorphInstantiation, RepNetworks }
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitudes, GenderIdentity, MorphModel, MorphInstance, RepNetwork }
import com.lkroll.ep.compendium.data.DefaultSkills

trait PathIndex {
  def tableIndex: Int;
}

sealed trait FirewallOption;
object FirewallOption {
  case object Skip extends FirewallOption;
  case object Allow extends FirewallOption;
  case object Always extends FirewallOption;
}

class LifePathCreation(
  val fair:     Boolean        = true,
  val firewall: FirewallOption = FirewallOption.Skip,
  val gear:     Boolean        = false,
  val moxie:    Boolean        = true) extends CreationSystem {

  import CharImplicits.skilldef2skill;
  import Implicits.RandomArray;

  override def label: String = "Life Path"
  override def randomCharacter(rand: Random): CreationResult = {
    var stages: List[StageResult] = List(SystemResult(this));

    val apts = AptitudeTemplateTable.roll(rand);
    stages ::= Stages.AptitudeTable(apts);

    val nativeLang = LanguageTable.roll(rand).copy(ranks = 70);
    stages ::= Stages.NativeLangTable(nativeLang);

    var skills: List[character.Skill] = List(nativeLang);

    var allPackages: List[PPPackage] = Nil;

    val childhood = YouthPathTable.roll(rand);
    stages ::= Stages.YouthPathTable(childhood);

    var history: List[String] = Nil;
    val skipFall = childhood.childhoods.head.skipPreFallReason match {
      case Some(r) => {
        history ::= s"Born after the Fall: $r";
        true
      }
      case None => false
    }
    history ::= childhood.youthType;
    history :::= childhood.childhoods.map(c => s"${c.label} (${c.group})");
    val background = childhood.childhoods.map(_.pkg.label).mkString(" & ");

    val genderId = if (background.contains("Infolife")) {
      agiGenderTable.randomElement(rand).get
    } else {
      normalGenderTable.randomElement(rand).get
    };

    var backgroundPackages: List[BackgroundPackage] = childhood.childhoods.map(_.pkg);
    assert(!backgroundPackages.isEmpty, "Can not have no background package!");
    val startingMorph = childhood.childhoods.head.startingMorph; // just take the first
    val instantiateMorph = (m: MorphModel) => MorphInstantiation.forModel(m, genderId).roll(rand);
    var currentMorph: MorphInstance = instantiateMorph(startingMorph);
    val backgroundEvent: Option[BackgroundEventResult] = if (skipFall) {
      None
    } else {
      val bge = BackgroundEvent.roll(rand);
      stages ::= Stages.BackgroundEvent(bge);
      history ::= bge.descr;
      bge.effect match {
        case BackgroundEventEffect.ReplacePackage(pkg) => {
          backgroundPackages.reverse match {
            case head :: rest => backgroundPackages = (pkg.ofLevel(head.level).get :: rest).reverse
            case Nil          => // s. above
          }
        }
        case BackgroundEventEffect.NewMorph(m) => currentMorph = instantiateMorph(m)
        case _                                 => () // take effect later
      }
      Some(bge)
    }
    allPackages ++= backgroundPackages;

    var char = character.CharGenCharacter(
      name = "Anonymous",
      gender = genderId,
      aptitudes = Aptitudes(base = apts.aptitudes, morphBoni = currentMorph.aptitudeBonus, morphMax = currentMorph.aptitudeMax),
      skills = skills,
      background = background,
      startingMorph = startingMorph,
      activeMorph = currentMorph,
      history = history.reverse);

    char = backgroundPackages.foldLeft(char)((acc, bgp) => bgp.applyTo(acc, rand));

    backgroundEvent match {
      case Some(bge) => bge.effect match {
        case BackgroundEventEffect.CharMod(mod) => char = mod.applyTo(char)
        case _                                  => () // already applied
      }
      case None => () // ignore
    }

    val skipPreFallAdult = if (!skipFall) {
      val startingAge = StartingAge.roll(rand);
      stages ::= Stages.StartingAge(startingAge);
      char = startingAge.mods.foldLeft(char.copy(age = startingAge.age)) { (acc, mod) =>
        mod.applyTo(acc)
      };
      startingAge.skipPreFall;
    } else {
      char = char.copy(age = rand.nextInt(10));
      true
    };
    val lastBGNextPath = childhood.childhoods.last.nextPath;
    val preFallPath = if (!skipPreFallAdult) {
      var adultPath = AdultPathTable.preFall(lastBGNextPath).roll(rand);
      val lifeEvent = PreFallLifeEvent.roll(rand);
      lifeEvent.effects.foreach {
        case PreFallLifeEventEffect.ReplacePath => adultPath = AdultPathTable.preFall(lastBGNextPath).roll(rand);
        case _                                  => () // apply later
      }
      stages ::= Stages.PreFallAdultPath(adultPath);
      val adultPackage: PPPackage = adultPath.pkg.left.map(_.basicPackage).merge;
      allPackages ::= adultPackage;
      char = adultPackage.applyTo(char, rand);
      stages ::= Stages.PreFallLifeEvent(lifeEvent);
      history ::= lifeEvent.descr;
      char = lifeEvent.effects.foldLeft(char) { (acc, effect) =>
        effect match {
          case PreFallLifeEventEffect.CharMod(mod)    => mod.applyTo(acc)
          case PreFallLifeEventEffect.NewMorph(morph) => acc.copy(activeMorph = instantiateMorph(morph))
          case PreFallLifeEventEffect.ReplacePath     => acc // already applied
        }
      }
      Some(adultPath.path)
    } else None;

    val (fallFocus, fallFaction) = if (!skipFall) {
      val fallEvent = FallEvent.roll(rand);
      stages ::= Stages.FallEvent(fallEvent);
      var faction = Option.empty[TwoPackageGroup[FactionPackage]];
      var focus = Option.empty[Either[ThreePackageGroup[FocusPackage], CustomizationPackage]];
      fallEvent.effects.foreach {
        case FallEventEffect.CharMod(mod)         => char = mod.applyTo(char)
        case FallEventEffect.NewMorph(morph)      => char = char.copy(activeMorph = instantiateMorph(morph))
        case FallEventEffect.PostFallFaction(pkg) => faction = Some(pkg)
        case FallEventEffect.PostFallFocus(pkg)   => focus = Some(pkg)
      }
      history ::= fallEvent.descr;
      (focus, faction)
    } else {
      (None, None)
    };
    val postFallTable = PostFallPathTable.withFallPackages(char.isAGI, char.isUplift, preFallPath, fallFocus, fallFaction)
    val postFallPath = postFallTable.roll(rand);
    stages ::= Stages.PostFallAdultPath(postFallPath);
    val adultPackage: PPPackage = postFallPath.adultPath.pkg.left.map(_.ofLevel(postFallPath.distribution.focus).get).merge;
    allPackages ::= adultPackage;
    char = adultPackage.applyTo(char, rand);
    val factionPackage = postFallPath.factionPath.pkg.ofLevel(postFallPath.distribution.faction).get;
    allPackages ::= factionPackage;
    char = factionPackage.applyTo(char, rand);
    val faction = s"${postFallPath.factionPath.pkg.label} (${postFallPath.factionPath.label})";
    char = char.copy(faction = faction);

    var extraPackages: List[PPPackage] = Nil;
    val postFallEvent = PostFallEvent.roll(rand);
    postFallEvent.effects.foreach {
      case PostFallEventEffect.ExtraPackage(pkg) => {
        extraPackages ::= pkg;
        allPackages ::= pkg;
      }
      case _ => () // apply later
    }
    if (fair) {
      val morePackages = MorePackagesTable.withPostFall(postFallTable, postFallPath.adultPath, char.isAsync);
      while (allPackages.map(_.ppCost).sum < 10) {
        val extraPackage: ExtraPackage = morePackages.roll(rand);
        if (allPackages.find(p => p.label.startsWith(extraPackage.prefix)).isEmpty) {
          extraPackages ::= extraPackage.pkg;
          allPackages ::= extraPackage.pkg;
        }
      }
    }
    stages ::= Stages.FinalPackages(extraPackages, allPackages.map(_.ppCost).sum);
    char = extraPackages.foldLeft(char)((acc, p) => p.applyTo(acc, rand));

    stages ::= Stages.PostFallEvent(postFallEvent);
    char = postFallEvent.effects.foldLeft(char) { (acc, e) =>
      e match {
        case PostFallEventEffect.CharMod(mod)    => mod.applyTo(acc)
        case PostFallEventEffect.NewMorph(morph) => acc.copy(activeMorph = instantiateMorph(morph))
        case PostFallEventEffect.ExtraPackage(_) => acc // already applied
      }
    };
    history ::= postFallEvent.descr;

    val hasIRep = char.rep.getOrElse(RepNetworks.iRep, 0) > 0;
    val hasNetFirewall = char.skills.find(s => DefaultSkills.networking.name == s.name && s.field.get == "Firewall").isDefined;
    firewall match {
      case FirewallOption.Allow => {
        if (hasIRep || hasNetFirewall) {
          if (!hasIRep) {
            char = char.copy(rep = char.rep + (RepNetworks.iRep -> 5)); // Firewall chars should have at least 5 i-Rep
          }
          val firewallEvent = FirewallEvent.roll(rand);
          stages ::= Stages.FirewallEvent(firewallEvent);
          char = firewallEvent.effects.foldLeft(char) { (acc, e) =>
            e match {
              case FirewallEventEffect.CharMod(mod)    => mod.applyTo(acc)
              case FirewallEventEffect.NewMorph(morph) => acc.copy(activeMorph = instantiateMorph(morph))
            }
          };
          history ::= firewallEvent.descr;
        }
      }
      case FirewallOption.Always => {
        if (!hasIRep) {
          char = char.copy(rep = char.rep + (RepNetworks.iRep -> 5)); // Firewall chars should have at least 5 i-Rep
        }
        val firewallEvent = FirewallEvent.roll(rand);
        stages ::= Stages.FirewallEvent(firewallEvent);
        char = firewallEvent.effects.foldLeft(char) { (acc, e) =>
          e match {
            case FirewallEventEffect.CharMod(mod)    => mod.applyTo(acc)
            case FirewallEventEffect.NewMorph(morph) => acc.copy(activeMorph = instantiateMorph(morph))
          }
        };
      }
      case FirewallOption.Skip => {
        import Implicits.RandomArray;
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

    val startingCredit = StartingCreditTable.roll(rand);
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

    stages ::= Stages.CombineEverything(combinationResult);

    val storyEvent = StoryEvent.roll(rand);
    stages ::= Stages.StoryEvent(storyEvent);
    history ::= storyEvent;

    CreationResult(char.copy(history = history.reverse), stages.reverse)
  }

  private val normalGenderTable: RollTable[GenderIdentity] = RollTable(
    (1 to 45) -> GenderIdentity.Male,
    (46 to 90) -> GenderIdentity.Female,
    (91 to 93) -> GenderIdentity.Genderless,
    (94 to 100) -> GenderIdentity.Other);

  private val agiGenderTable: RollTable[GenderIdentity] = RollTable(
    (1 to 10) -> GenderIdentity.Male,
    (11 to 20) -> GenderIdentity.Female,
    (21 to 80) -> GenderIdentity.Genderless,
    (81 to 99) -> GenderIdentity.Plurality,
    (100 to 100) -> GenderIdentity.Other);
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

  case class YouthPathTable(result: YouthPath) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 3: Youth Path");
      renderer.labelled("Type", result.youthType);
      result.childhoods.foreach { c =>
        renderer.section(c.label);
        renderer.note(c.group);
        renderer.newline();
        renderer.labelled("Background Package", c.pkg.label);
        renderer.raw(", ");
        renderer.labelled("Starting Morph", c.startingMorph.name);
        renderer.raw(", ");
        renderer.labelled("Next Path", s"Table 6.${c.nextPath.i}");
        renderer.raw("\n");
      }
    }
  }

  case class BackgroundEvent(result: BackgroundEventResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 4: Background Event");
      renderer.value(result.descr);
      renderer.section("Effects")
      renderer.list(result.effectList);
    }
  }

  case class PreFallLifeEvent(result: PreFallLifeEventResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 7: Pre-Fall Life Event");
      renderer.value(result.descr);
      renderer.section("Effects")
      renderer.list(result.effectList);
    }
  }

  case class FallEvent(result: FallEventResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 8: Fall Event");
      renderer.value(result.descr);
      renderer.section("Effects")
      renderer.list(result.effectList);
    }
  }

  case class PostFallEvent(result: PostFallEventResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 11: Post-Fall Life Event");
      renderer.value(result.descr);
      renderer.section("Effects")
      renderer.list(result.effectList);
    }
  }

  case class StoryEvent(result: String) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 16: Story Event");
      renderer.value(result);
    }
  }

  case class FirewallEvent(result: FirewallEventResult) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 12: Firewall Event");
      renderer.value("Character has joined Firewall");
      renderer.raw(": ");
      renderer.value(result.descr);
      renderer.section("Effects")
      renderer.list(result.effectList);
    }
  }

  case class StartingAge(result: StartingAgeResult) extends StageResult {
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

  case class PreFallAdultPath(result: AdultPath) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 6: Adult Pre-Fall Path");
      renderer.labelled("Type", result.kind);
      renderer.newline();
      renderer.value(s"From Table 6.${result.path.index}: ${result.path.label}");
      renderer.newline();
      val pkgName = result.pkg.left.map(_.basicPackage).merge.label;
      renderer.labelled("Focus Package", pkgName);
      renderer.newline();
      renderer.value(s"Faction Path: Table 9.${result.path.faction.i}")
    }
  }

  case class PostFallAdultPath(result: PostFallPath) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 9: Adult Post-Fall Path");
      renderer.labelled("Distribution Type", result.distribution.kind);
      renderer.section("Focus Path");
      renderer.labelled("Type", result.adultPath.kind);
      renderer.newline();
      renderer.value(s"From Table 6.${result.adultPath.path.index}: ${result.adultPath.path.label}");
      renderer.newline();
      val pkgName = result.adultPath.pkg.left.map(_.ofLevel(result.distribution.focus).get).merge.label;
      renderer.labelled("Focus Package", pkgName);
      renderer.section("Faction Path");
      renderer.labelled("Type", result.factionPath.kind);
      renderer.newline();
      renderer.value(s"From Table 9.${result.factionPath.index}: ${result.factionPath.label}");
      renderer.newline();
      renderer.labelled("Faction Package", result.factionPath.pkg.ofLevel(result.distribution.faction).get.label);
    }
  }
  case class FinalPackages(result: List[PPPackage], ppTotal: Int) extends StageResult {
    def render(renderer: Renderer): Unit = {
      renderer.stage("Step 10: Final Packages");
      renderer.value(s"Added ${result.size} packages.");
      renderer.newline();
      renderer.labelled("Total PP", ppTotal.toString());
      renderer.section("Extra Packages");
      renderer.list(result.map(_.label));
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

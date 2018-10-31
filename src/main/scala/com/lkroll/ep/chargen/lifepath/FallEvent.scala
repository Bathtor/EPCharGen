package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitude, Effect, EPTrait, MorphModel, MorphType, SkillCategory }
import com.lkroll.ep.compendium.data._

case class FallEventResult(descr: String, effects: List[FallEventEffect]) {
  def effectList: List[String] = effects.map(_.render);
}
sealed trait FallEventEffect {
  def render: String;
}
object FallEventEffect {
  case class CharMod(mod: CharacterMod) extends FallEventEffect {
    override def render: String = mod.render;
  }
  case class NewMorph(morph: MorphModel) extends FallEventEffect {
    override def render: String = s"Changed Morph to ${morph.name}";
  }
  case class PostFallFocus(pkg: Either[ThreePackageGroup[FocusPackage], CustomizationPackage]) extends FallEventEffect {
    override def render: String = {
      val pkgLabel = pkg match {
        case Left(group) => group.label
        case Right(cp)   => cp.label
      };
      s"Acquired Focus ${pkgLabel}";
    }
  }
  case class PostFallFaction(pkg: TwoPackageGroup[FactionPackage]) extends FallEventEffect {
    override def render: String = s"Joined Faction ${pkg.label}";
  }
}

object FallEvent extends Table {
  import Implicits.RandomArray
  import CharImplicits._;
  import DefaultSkills._;
  import PackageImplicits.{ Moxie, StartingCredit, morphT2filter };

  override type Result = FallEventResult;

  sealed trait FallEventEntry {
    def resolve(rand: Random): FallEventResult;
  }
  case class CharMod(descr: String, mod: CharacterMod) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      FallEventResult(descr, List(FallEventEffect.CharMod(mod)))
    }
  }
  case class RandCharMod(descr: String, mod: Random => CharacterMod) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      FallEventResult(descr, List(FallEventEffect.CharMod(mod(rand))))
    }
  }
  case class CharMods(descr: String, mods: List[CharacterMod]) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      val effects = mods.map(mod => FallEventEffect.CharMod(mod));
      FallEventResult(descr, effects)
    }
  }
  case class RandCharMods(descr: String, mods: Random => List[CharacterMod]) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      val effects = mods(rand).map(mod => FallEventEffect.CharMod(mod));
      FallEventResult(descr, effects)
    }
  }
  case class StoryEventMod(mod: CharacterMod) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      val story = StoryEvent.roll(rand);
      FallEventResult(s"You somehow stay safe and untouched by the chaos and horror. $story", List(FallEventEffect.CharMod(mod)))
    }
  }
  case class RandAptMod(descr: String, mod: Int) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      val apt = Aptitude.values.toArray.randomElement(rand).get;
      FallEventResult(descr, List(FallEventEffect.CharMod(CharacterMod.AptitudeMod(apt, mod))))
    }
  }
  case class TakeFocus(descr: String, pkg: Either[ThreePackageGroup[FocusPackage], CustomizationPackage]) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      FallEventResult(descr, List(FallEventEffect.PostFallFocus(pkg)))
    }
  }
  object TakeFocus {
    def apply(descr: String, pkg: ThreePackageGroup[FocusPackage]): TakeFocus = TakeFocus(descr, Left(pkg));
    def apply(descr: String, pkg: CustomizationPackage): TakeFocus = TakeFocus(descr, Right(pkg));
  }
  case class TakeFaction(descr: String, pkg: TwoPackageGroup[FactionPackage]) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      FallEventResult(descr, List(FallEventEffect.PostFallFaction(pkg)))
    }
  }
  case class RandMorph(descr: String, morph: Random => MorphModel) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      FallEventResult(descr, List(FallEventEffect.NewMorph(morph(rand))))
    }
  }
  case class MultipleEntries(descr: String, entries: FallEventEntry*) extends FallEventEntry {
    override def resolve(rand: Random): FallEventResult = {
      val effects = entries.foldLeft(List.empty[FallEventEffect]) { (acc, entry) =>
        entry.resolve(rand).effects ::: acc
      };
      FallEventResult(descr, effects)
    }
  }

  private val data: RollTable[FallEventEntry] = RollTable(
    (1 to 10) -> StoryEventMod(CharacterMod.Moxie(1)),
    (11 to 12) -> MultipleEntries("You stay to the end, fighting a rear-guard action.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", Moxie + 1)),
    (13 to 14) -> TakeFaction("You are trapped on Earth when the homeworld is interdicted and quarantined.", FactionPackages.earthSurvivor),
    (15 to 16) -> TakeFocus("You exemplify yourself in destroying TITAN machines.", FocusPackages.wrecker),
    (17 to 18) -> TakeFocus("Exposure to the Watts-MacLeod exsurgent virus opens your awareness to aspects of the world of which others are blind.", FocusPackages.savantAsync),
    (19 to 20) -> TakeFocus("Infected with the Watts-MacLeod exsurgent virus, you learn to use your new powers forcefully.", FocusPackages.controllerAsync),
    (21 to 22) -> TakeFocus("After you are infected, you are trained to use your psi to kill.", FocusPackages.combatAsync),
    (23 to 24) -> TakeFocus("Infection permanently changes your perceptions.", FocusPackages.scannerAsync),
    (25 to 26) -> CharMod("You rack up a major debt to pay a bribe to get yourself off-world.", TraitsNegativeTranshuman.debtLevel3),
    (27 to 28) -> MultipleEntries("You assume that you died on Earth, but you don’t know for sure.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", TraitsNegativeEP.editedMemories)),
    (29 to 30) -> RandMorph("You know that the TITANs killed or uploaded you, according to reports.", ChoosingAMorph.randMorph),
    (31 to 32) -> RandMorph("You fall victim to the TITANs in Earth orbit, on Luna, or on Mars.", ChoosingAMorph.randMorph),
    (33 to 34) -> RandMorph("You are slain, not by the TITANs,but by hostile action from a rival government,f action, or hypercorp.", ChoosingAMorph.randMorph),
    (35 to 36) -> RandMorph("You hide your body away in cold storage on Earth before farcasting off to safety.", ChoosingAMorph.randMorph),
    (37 to 38) -> MultipleEntries("Your ego escapes the devastation of Earth, only to be locked away in cold storage for years. You are only recently resleeved.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", TraitsNegativeEP.realWorldNaivete)),
    (39 to 40) -> MultipleEntries("Your ego survives the Fall but is locked in simulspace for years before you are resleeved.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", interfacing + 20)),
    (41 to 42) -> MultipleEntries("After escaping the Fall, you are forced into indentured service before you are resleeved.", RandMorph("", ChoosingAMorph.randOnly(_, MorphType.Synthmorph)), RandCharMod("", Skills.modOnly(_, 20, SkillCategory.Technical))),
    (43 to 44) -> CharMod("You do what you can to help, but you still lose almost everyone in your life.", Moxie + 1),
    (45 to 46) -> MultipleEntries("You heroically sacrifice yourself so that others can escape.", RandMorph("", ChoosingAMorph.randMorph), RandCharMod("", RepNetworks.chooseAny(_, 10))),
    (47 to 48) -> CharMod("You risk your life in a desperate holding action.", TraitsPositiveEP.brave),
    (49 to 50) -> TakeFocus("You are infected ... but get better.", CustomizationPackages.async),
    (51 to 52) -> CharMod("You learn the hard way how susceptible you are to exsurgent influence.", TraitsNegativeEP.psyVulnerability),
    (53 to 54) -> TakeFocus("You encounter an exsurgent and are permanently changed by the experience. ", CustomizationPackages.asyncAdept),
    (55 to 56) -> CharMod("The near extinction of your species hardens your resolve.", Aptitude.WIL + 5),
    (57 to 58) -> CharMod("You hide and survive an encounter with exsurgents where others die.", TraitsPositiveEP.psiChameleon),
    (59 to 60) -> CharMod("You end up in possession of a rare Earth artifact worth 1d10 x 10,000 credits.", CharacterMod.GainGear(UniqueGear("a rare Earth artifact worth 1d10 x 10,000 credits"))),
    (61 to 62) -> CharMod("You make a bad call that gets people killed. Now you question your gut feelings.", Aptitude.INT - 5),
    (63 to 64) -> MultipleEntries("You were horrified at the idea of resleeving, but the alternative seemed worse—or so you thought.", CharMod("", TraitsNegativeEP.morphingDisorderLevel3), RandMorph("", ChoosingAMorph.randMorph)),
    (65 to 66) -> CharMod("A nanoviral infection leaves you permanently damaged. ", TraitsNegativeEP.neuralDamage),
    (67 to 68) -> MultipleEntries("You have unfortunate memories of some ... thing ... eating your face off.", CharMod("", TraitsNegativeEP.timid), RandMorph("", ChoosingAMorph.randMorph)),
    (69 to 70) -> CharMod("You witness unspeakable horrors during the Fall, standing idly by while others die.", TraitsNegativeEP.combatParalysis),
    (71 to 72) -> CharMod("Several near-death experiences hone your reflexes.", Aptitude.REF + 5),
    (73 to 74) -> CharMod("Your relatives die, but you are left as the sole heir to the family’s modest wealth that made it off-world.", StartingCredit + 25000),
    (75 to 76) -> CharMod("You will never get the image of headhunter drones at work out of your mind.", TraitsNegativeEP.mentalDisorder.copy(name = "Mental Disorder (PTSD)")),
    (77 to 78) -> CharMod("You exhibit natural leadership in a time of crisis.", Skills.oneOf(persuasion, intimidation) + 10),
    (79 to 80) -> CharMod("The only way you can cope with the loss of your former life is through drugs.", TraitsNegativeTranshuman.drugFiend),
    (81 to 82) -> CharMod("You cope with the horrors you experience in the midst of evacuation the only way you could—by postponing the trauma until you are safe.", TraitsPositiveTranshuman.traumaToleranceLevel1),
    (83 to 84) -> MultipleEntries("Your willingness to profit from others’ misery gains you respect in some circles.", CharMod("", RepNetworks.gRep + 10), RandCharMod("", RepNetworks.chooseAny(_, -5))),
    (85 to 86) -> CharMod("You experience things during the Fall that would leave others a shattered mess.", TraitsPositiveTranshuman.hardening),
    (87 to 88) -> CharMod("You lose everything–and nearly lose your mind as well. It will never recover to its former strength.", TraitsNegativeTranshuman.frailSanityLevel1),
    (89 to 90) -> MultipleEntries("Not only do you die during the Fall, your backups are lost as well. You live on as a beta fork of your original self.", CharMod("", TraitsNegativeTranshuman.beta), RandMorph("", ChoosingAMorph.randMorph)),
    (91 to 92) -> CharMod("You lose a fortune during the Fall.", StartingCredit - 10000),
    (93 to 94) -> MultipleEntries("You die during the Fall, but that doesn’t stop you from going to die again, and then again, and then again some more.", CharMod("", TraitsPositiveTranshuman.phoenixLevel1), RandMorph("", ChoosingAMorph.randMorph)),
    (95 to 96) -> CharMod("After committing a crime, you are sentenced to indentured service, but the Fall lets you skip out.", TraitsNegativeTranshuman.defferedIndentureLevel2),
    (97 to 98) -> CharMod("You rack up an impressive kill score fighting TITAN machines.", TraitsPositiveTranshuman.tacnetSniper),
    (99 to 100) -> CharMod("Your willingness to make lives a priority over material things earns you respect.", RepNetworks.circleARep + 10));

  override def label: String = "Fall Event";
  override def source: String = "Transhuman p.63-64";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get.resolve(rand)
  }
}

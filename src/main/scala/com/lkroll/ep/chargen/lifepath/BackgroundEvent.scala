package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitude, Effect, EPTrait, MorphModel }
import com.lkroll.ep.compendium.data._

case class BackgroundEventResult(descr: String, effect: BackgroundEventEffect) {
  def effectList: List[String] = List(effect.render);
}
sealed trait BackgroundEventEffect {
  def render: String;
}
object BackgroundEventEffect {
  case class CharMod(mod: CharacterMod) extends BackgroundEventEffect {
    override def render: String = mod.render;
  }
  case class NewMorph(morph: MorphModel) extends BackgroundEventEffect {
    override def render: String = s"Changed Morph to ${morph.name}";
  }
  case class ReplacePackage(pkg: ThreePackageGroup[BackgroundPackage]) extends BackgroundEventEffect {
    override def render: String = s"Replaced last background package with ${pkg.label}";
  }
}

object BackgroundEvent extends Table {
  import CharImplicits._;
  import Skills.Defaults._

  override type Result = BackgroundEventResult;

  sealed trait BGEventEntry {
    def resolve(rand: Random): BackgroundEventResult;
  }
  case class CharMod(descr: String, mod: CharacterMod) extends BGEventEntry {
    override def resolve(rand: Random): BackgroundEventResult = {
      BackgroundEventResult(descr, BackgroundEventEffect.CharMod(mod))
    }
  }
  case class RandCharMod(descr: String, mod: Random => CharacterMod) extends BGEventEntry {
    override def resolve(rand: Random): BackgroundEventResult = {
      BackgroundEventResult(descr, BackgroundEventEffect.CharMod(mod(rand)))
    }
  }
  case class StoryEventMod(mod: CharacterMod) extends BGEventEntry {
    override def resolve(rand: Random): BackgroundEventResult = {
      val story = StoryEvent.roll(rand);
      BackgroundEventResult(story, BackgroundEventEffect.CharMod(mod))
    }
  }
  case class ReplacePackage(descr: String, pkg: ThreePackageGroup[BackgroundPackage]) extends BGEventEntry {
    override def resolve(rand: Random): BackgroundEventResult = {
      BackgroundEventResult(descr, BackgroundEventEffect.ReplacePackage(pkg))
    }
  }
  case class RandMorph(descr: String, morph: Random => MorphModel) extends BGEventEntry {
    override def resolve(rand: Random): BackgroundEventResult = {
      BackgroundEventResult(descr, BackgroundEventEffect.NewMorph(morph(rand)))
    }
  }

  private val data: RollTable[BGEventEntry] = RollTable(
    (1 to 20) -> StoryEventMod(CharacterMod.Moxie(1)),
    (21 to 22) -> CharMod("Traumatic childhood accident.", TraitsNegativeEP.mentalDisorder.copy(name = "Mental Disorder (PTSD)")),
    (23 to 24) -> CharMod("You are not only good with your hands, you are good with both hands.", TraitsPositiveEP.ambidextrous),
    (25 to 26) -> CharMod("The people that raise you do not tolerate foolishness.", TraitsPositiveEP.commonSense),
    (27 to 28) -> CharMod("You are raised in an abusive environment", TraitsPositiveEP.painTolerance1),
    (29 to 30) -> ReplacePackage("You fall in with the wrong crowd.", BackgroundPackages.streetRat),
    (31 to 32) -> CharMod("You are kept at home and not allowed to play sports or with other kids.", Aptitude.COO - 5),
    (33 to 34) -> CharMod("You grow up in a maze-like urban area or a difficult rural area.", TraitsPositiveEP.directionSense),
    (35 to 36) -> CharMod("You are raised in dangerous conditions where you have to adapt or die.", TraitsPositiveEP.fastLearner),
    (37 to 38) -> RandCharMod("You skip too much school.", Skills.modAny(_, -20)),
    (39 to 40) -> CharMod("Growing up isolated from others, you develop introvert tendencies.", Aptitude.SAV - 5),
    (41 to 42) -> CharMod("While your friends throw themselves into VR gaming worlds, you simply throw up.", TraitsNegativeEP.vrVertigo),
    (43 to 44) -> CharMod("Raised in an environment of constant stimulus, you are forced to tune out.", TraitsNegativeEP.oblivious),
    (45 to 46) -> CharMod("You grow up in a melting-pot, polyglot culture.", TraitsPositiveEP.hyperLinguist),
    (47 to 48) -> RandCharMod("You party too hard", Skills.modAny(_, -20)),
    (49 to 50) -> CharMod("Your childhood education is poor to non-existent.", TraitsNegativeEP.illiterate),
    (51 to 52) -> CharMod("A healthy amount of physical activities improves your abilities.", Aptitude.COO + 5),
    (53 to 54) -> RandCharMod("One (or both) of you parents is bilingual.", language.modAnyField(_, +20)),
    (55 to 56) -> CharMod("A misunderstood situation makes you the laughing stock of your peer group.", TraitsNegativeTranshuman.sociallyGraceless),
    (57 to 58) -> CharMod("Shoplifting and stealing are either hobbies or a necessity.", palming + 10),
    (59 to 60) -> CharMod("You experiment with drugs, but it isn't for you", TraitsPositiveTranshuman.drugException),
    (61 to 62) -> CharMod("A dysfunctional home life keeps you from doing well in school.", Aptitude.COG - 5),
    (63 to 64) -> CharMod("Being a bully has its advantages.", intimidation + 10),
    (65 to 66) -> CharMod("Your first experiments with forking prove to you that merging will not be easy.", TraitsNegativeTranshuman.divergentPersonality),
    (67 to 68) -> RandMorph("You receive your first cortical stack at an early age -- and luckily just in time.", ChoosingAMorph.randMorph),
    (69 to 70) -> CharMod("You are raised around animals.", Skills.Defaults.animalHandling + 10),
    (71 to 72) -> CharMod("Your parents raise you with some unusual ideas.", TraitsNegativeTranshuman.faultyEducation),
    (73 to 74) -> RandCharMod("You cheat your way through school.", Skills.modAny(_, -10)),
    (75 to 76) -> CharMod("You enjoy urban exploration and getting into off-limits areas", Skills.oneOf(climbing, infiltration) + 10),
    (77 to 78) -> CharMod("You hone your headshot skills in VR combat simulations.", TraitsPositiveTranshuman.murderSimulatorAddict),
    (79 to 80) -> CharMod("Because you excel in your studies, you are placed in an advanced program,", Aptitude.COG + 5),
    (81 to 82) -> CharMod("Your parents are highly private and extremely strict.", TraitsNegativeTranshuman.poorlySocialized),
    (83 to 84) -> CharMod("You leave home at an early age and forge your own path.", CharacterMod.Moxie(1)),
    (85 to 86) -> CharMod("One of your personality quirks gains you some attention from peers, but then it becomes a permanent and noticeable part of your daily behaviour.", TraitsNegativeTranshuman.identifiablyQuirk),
    (87 to 88) -> CharMod("As a free-range kid, you learn how to get around on your own.", navigation + 10),
    (89 to 90) -> CharMod("One of your parental figures abandons you as a child.", TraitsNegativeTranshuman.trustingHeart),
    (91 to 92) -> CharMod("A series of childhood injuries leaves you struggling to catch up physically.", Aptitude.SOM - 5),
    (93 to 94) -> CharMod("You lose all of your close friends in a horribly awkward teen social situation.", TraitsNegativeTranshuman.notATeamPlayer),
    (95 to 96) -> CharMod("Tormented by bullies as a kid, you learn to stand up for yourself.", unarmedCombat + 10),
    (97 to 98) -> CharMod("The strictness of your parents leaves you only one choice: to excel at lying.", deception + 10),
    (99 to 100) -> CharMod("Adult generations are never quite as on-top of technological changes, and you use that to your advantage.", infosec + 10));

  override def label: String = "Background Event";
  override def source: String = "Transhuman p.59";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get.resolve(rand)
  }
}

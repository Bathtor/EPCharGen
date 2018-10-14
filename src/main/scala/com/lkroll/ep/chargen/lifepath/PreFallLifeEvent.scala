package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitude, Effect, EPTrait, MorphModel, MorphType }
import com.lkroll.ep.compendium.data._

case class PreFallLifeEventResult(descr: String, effects: List[PreFallLifeEventEffect]) {
  def effectList: List[String] = effects.map(_.render);
}
sealed trait PreFallLifeEventEffect {
  def render: String;
}
object PreFallLifeEventEffect {
  case class CharMod(mod: CharacterMod) extends PreFallLifeEventEffect {
    override def render: String = mod.render;
  }
  case class NewMorph(morph: MorphModel) extends PreFallLifeEventEffect {
    override def render: String = s"Changed Morph to ${morph.name}";
  }
  case object ReplacePath extends PreFallLifeEventEffect {
    override def render: String = "Replaced last path.";
  }
}

object PreFallLifeEvent extends Table {
  import Implicits.RandomArray
  import CharImplicits._;
  import Skills.Defaults._;
  import PackageImplicits.{ Moxie, StartingCredit, morphT2filter };

  override type Result = PreFallLifeEventResult;

  sealed trait PFLEventEntry {
    def resolve(rand: Random): PreFallLifeEventResult;
  }
  case class CharMod(descr: String, mod: CharacterMod) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      PreFallLifeEventResult(descr, List(PreFallLifeEventEffect.CharMod(mod)))
    }
  }
  case class RandCharMod(descr: String, mod: Random => CharacterMod) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      PreFallLifeEventResult(descr, List(PreFallLifeEventEffect.CharMod(mod(rand))))
    }
  }
  case class CharMods(descr: String, mods: List[CharacterMod]) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      val effects = mods.map(mod => PreFallLifeEventEffect.CharMod(mod));
      PreFallLifeEventResult(descr, effects)
    }
  }
  case class RandCharMods(descr: String, mods: Random => List[CharacterMod]) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      val effects = mods(rand).map(mod => PreFallLifeEventEffect.CharMod(mod));
      PreFallLifeEventResult(descr, effects)
    }
  }
  case class StoryEventMod(mod: CharacterMod) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      val story = StoryEvent.roll(rand);
      PreFallLifeEventResult(story, List(PreFallLifeEventEffect.CharMod(mod)))
    }
  }
  case class RandAptMod(descr: String, mod: Int) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      val apt = Aptitude.values.toArray.randomElement(rand).get;
      PreFallLifeEventResult(descr, List(PreFallLifeEventEffect.CharMod(CharacterMod.AptitudeMod(apt, mod))))
    }
  }
  case class ReplacePath(descr: String) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      PreFallLifeEventResult(descr, List(PreFallLifeEventEffect.ReplacePath))
    }
  }
  case class RandMorph(descr: String, morph: Random => MorphModel) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      PreFallLifeEventResult(descr, List(PreFallLifeEventEffect.NewMorph(morph(rand))))
    }
  }
  case class MultipleEntries(descr: String, entries: PFLEventEntry*) extends PFLEventEntry {
    override def resolve(rand: Random): PreFallLifeEventResult = {
      val effects = entries.foldLeft(List.empty[PreFallLifeEventEffect]) { (acc, entry) =>
        entry.resolve(rand).effects ::: acc
      };
      PreFallLifeEventResult(descr, effects)
    }
  }

  private val data: RollTable[PFLEventEntry] = RollTable(
    (1 to 20) -> StoryEventMod(CharacterMod.Moxie(1)),
    (21 to 22) -> CharMod("You save an animal from danger.", TraitsPositiveEP.animalEmpathy),
    (23 to 24) -> CharMod("You take up a sport.", Skills.oneOf(climbing, fray, freeFall, freerunning, swimming) + 10),
    (25 to 26) -> CharMod("Your inability to improve hold you back from an important promotion/advancement", TraitsNegativeEP.slowLearner),
    (27 to 28) -> CharMod("You simply are not very comfortable with that whole resleeving thing.", TraitsNegativeEP.morphingDisorderLevel1),
    (29 to 30) -> RandCharMod("You are not a slacker. You take on part-time jobs or additional training.", rand => Skills.modAny(rand, +20)),
    (31 to 32) -> RandCharMods("You travel extensively", rand => List(language.modAnyField(rand, +10), language.modAnyField(rand, +10))),
    (33 to 34) -> CharMod("Regular attention to your health and exercise improves your abilities", Aptitude.SOM + 5),
    (35 to 36) -> RandMorph("You decide you want to experiment", rand => ChoosingAMorph.randIgnore(rand, MorphType.Pod)),
    (37 to 38) -> ReplacePath("You are fired and your new career hopes are now dashed."),
    (39 to 40) -> RandCharMod("You pick up a new hobby", rand => (Skills.oneOf(art, interest) + 20).copy(field = Right(CharacterMod.SkillChoice.PickAny(rand)))),
    (41 to 42) -> RandMorph("An unfortunate habitat failure, traffic accident, or fire ends your life.", rand => ChoosingAMorph.randIgnore(rand, MorphType.Pod, MorphFilter.Uplift)),
    (43 to 44) -> CharMod("You travel extensively", navigation + 20),
    (45 to 46) -> RandMorph("Your work requires you to change your morph.", rand => ChoosingAMorph.randIgnore(rand, MorphType.Pod, MorphFilter.Uplift)),
    (47 to 48) -> RandAptMod("Your curiosity gets the better of you.", -5),
    (49 to 50) -> CharMod("You lose a limb in a traumatic incident, but grow it back.", Moxie + 1),
    (51 to 52) -> CharMod("An experience with still-unrefined psychosurgery leaves you forever altered.", TraitsNegativeTranshuman.anomalousMind),
    (53 to 54) -> CharMod("Something goes seriously glitchy with your muse, and you are nearly hurt as a result.", TraitsNegativeTranshuman.phobiaDisorder.copy(name = "Phobia Disorder (Muse)")),
    (55 to 56) -> MultipleEntries("You are a pioneer in the practice of egocasting.", CharMod("", TraitsPositiveTranshuman.egoPlasticityLevel1), RandMorph("", rand => ChoosingAMorph.randMorph(rand))),
    (57 to 58) -> CharMod("You save someone from drowning.", swimming + 10),
    (59 to 60) -> CharMod("You experiment with minor self-modification.", TraitsNegativeEP.modifiedBehaviourLevel1),
    (61 to 62) -> CharMod("You exhibit a serious lack of willpower in coping with your adult life.", TraitsNegativeEP.addictionMajor),
    (63 to 64) -> MultipleEntries("You experience a deadly vehicle accident.", CharMod("", StartingCredit - 10000), RandMorph("", rand => ChoosingAMorph.randMorph(rand))),
    (65 to 66) -> CharMod("A hacker friend shows you a few things, and then you show them a few things.", TraitsPositiveTranshuman.intuitiveCrackerLevel1),
    (67 to 68) -> CharMod("The poor state of affairs on Earth before the Fall impacts you heavily.", TraitsNegativeEP.mentalDisorder.copy(name = "Mental Disorder (Depression)")),
    (69 to 70) -> CharMod("You spend some time in some of the rougher, crisis-impacted areas on Earth before the Fall.", fray + 10),
    (71 to 72) -> CharMod("A period of poverty leaves you with the skills to get by.", scrounging + 10),
    (73 to 74) -> CharMod("You are implicated in a news-making scandal", RepNetwork.cRep - 10),
    (75 to 76) -> RandCharMod("You pick up a new hobby", rand => (Skills.oneOf(art, interest) + 10).copy(field = Right(CharacterMod.SkillChoice.PickAny(rand)))),
    (77 to 78) -> CharMod("You are an early adopter of psychosurgery, finding that your mind adapts well to changes.", TraitsPositiveTranshuman.malleableMindLevel1),
    (79 to 80) -> CharMod("The stress of rapid technological change overwhelms you.", TraitsNegativeEP.mentalDisorder.copy(name = "Mental Disorder (ADHD)")),
    (81 to 82) -> RandCharMod("You study or train hard.", Skills.specializeAny),
    (83 to 84) -> CharMod("You take up martial arts training.", unarmedCombat + 10),
    (85 to 86) -> RandCharMod("You choose your own path over what others tell you to do.", rand => Skills.modAny(rand, 10)), // TODO replace skill instead
    (87 to 88) -> CharMod("You take care of someone in a way that makes them obligated to help you out for a long time to come.", TraitsPositiveTranshuman.personalConnection),
    (89 to 90) -> CharMod("You learn the hard way that ignoring money management lessons was a bad idea.", StartingCredit - 10000),
    (91 to 92) -> CharMods("You take the fall for a crime you may or may not have been complicit in.", List(RepNetwork.cRep - 10, Moxie + 1)),
    (93 to 94) -> CharMod("An accident on a space elevator leaves your fearful of space.", TraitsNegativeTranshuman.phobiaDisorder.copy(name = "Phobia Disorder (Microgravity)")),
    (95 to 96) -> RandCharMod("You start up an unusual hobby.", rand => (Skills.oneOf(exoticMeleeWeapon, exoticRangedWeapon) + 20).copy(field = Right(CharacterMod.SkillChoice.PickAny(rand)))),
    (97 to 98) -> RandMorph("You lose a bet and spend a month sleeved in a pod or uplift morph before it was cool.", rand => ChoosingAMorph.randOnly(rand, MorphType.Pod, MorphFilter.Uplift)),
    (99 to 100) -> CharMod("You make some life decisions that prove prescient after the Fall", StartingCredit + 20000));

  override def label: String = "Pre-Fall Life Event";
  override def source: String = "Transhuman p.62";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get.resolve(rand)
  }
}

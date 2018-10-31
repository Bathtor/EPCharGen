package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitude, Cost, Effect, EPTrait, MorphModel, MorphType, Gear => DataGear, SkillCategory }
import com.lkroll.ep.compendium.data._

case class PostFallEventResult(descr: String, effects: List[PostFallEventEffect]) {
  def effectList: List[String] = effects.map(_.render);
}
sealed trait PostFallEventEffect {
  def render: String;
}
object PostFallEventEffect {
  case class CharMod(mod: CharacterMod) extends PostFallEventEffect {
    override def render: String = mod.render;
  }
  case class NewMorph(morph: MorphModel) extends PostFallEventEffect {
    override def render: String = s"Changed Morph to ${morph.name}";
  }
  case class ExtraPackage(pkg: PPPackage) extends PostFallEventEffect {
    override def render: String = s"Acquired package ${pkg.label}";
  }
}

object PostFallEvent extends Table {
  import Implicits.RandomArray
  import CharImplicits._;
  import DefaultSkills._;
  import PackageImplicits.{ Moxie, StartingCredit, morphT2filter };

  override type Result = PostFallEventResult;

  sealed trait PostFallEventEntry {
    def resolve(rand: Random): PostFallEventResult;
  }
  case class CharMod(descr: String, mod: CharacterMod) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.CharMod(mod)))
    }
  }
  case class RandCharMod(descr: String, mod: Random => CharacterMod) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.CharMod(mod(rand))))
    }
  }
  case class CharMods(descr: String, mods: List[CharacterMod]) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      val effects = mods.map(mod => PostFallEventEffect.CharMod(mod));
      PostFallEventResult(descr, effects)
    }
  }
  case class RandCharMods(descr: String, mods: Random => List[CharacterMod]) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      val effects = mods(rand).map(mod => PostFallEventEffect.CharMod(mod));
      PostFallEventResult(descr, effects)
    }
  }
  case class GatecrashingEvent(descr: String) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      val story = StoryEvent.roll(rand); // TODO Gatecrashing table!
      PostFallEventResult(s"$descr $story", List.empty)
    }
  }
  case class Story(descr: String) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List.empty)
    }
  }
  case class RandAptMod(descr: String, mod: Int) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      val apt = Aptitude.values.toArray.randomElement(rand).get;
      PostFallEventResult(descr, List(PostFallEventEffect.CharMod(CharacterMod.AptitudeMod(apt, mod))))
    }
  }
  case class RandMorph(descr: String, morph: Random => MorphModel) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.NewMorph(morph(rand))))
    }
  }
  case class MultipleEntries(descr: String, entries: PostFallEventEntry*) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      val effects = entries.foldLeft(List.empty[PostFallEventEffect]) { (acc, entry) =>
        entry.resolve(rand).effects ::: acc
      };
      PostFallEventResult(descr, effects)
    }
  }

  case class Gear(descr: String, item: DataGear) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.CharMod(CharacterMod.GainGear(item))))
    }
  }

  case class RandGear(descr: String, item: Random => DataGear) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.CharMod(CharacterMod.GainGear(item(rand)))))
    }
  }
  case class ExtraPackage(descr: String, pkg: PPPackage) extends PostFallEventEntry {
    override def resolve(rand: Random): PostFallEventResult = {
      PostFallEventResult(descr, List(PostFallEventEffect.ExtraPackage(pkg)))
    }
  }

  private val data: RollTable[PostFallEventEntry] = RollTable(
    (1 to 3) -> GatecrashingEvent("You are hired by a wealthy private party for some exclusive exoplanet missions."),
    (4 to 4) -> RandCharMods("You reinvent yourself.", rand => List(StartingAge.AptitudeTables.any(+5).randomElement(rand).get, StartingAge.AptitudeTables.any(-5).randomElement(rand).get)),
    (5 to 6) -> GatecrashingEvent("Your employer/collective sends you on a gatecrashing op."),
    (7 to 7) -> CharMod("You score an achievement that leaves you indelibly marked in your faction’s consciousness.", TraitsPositiveTranshuman.untarnishedReputation),
    (8 to 8) -> CharMod("You create a major diplomatic incident.", TraitsNegativeEP.blackMarkLevel3),
    (9 to 9) -> RandMorph("You fall victim to a terrorist attack or factional dispute.", ChoosingAMorph.randMorph),
    (10 to 10) -> CharMod("The merging of an overdue fork goes poorly, and coincidental inaccessible backups leave you permanently changed.", TraitsNegativeTranshuman.botchedMerge),
    (11 to 13) -> GatecrashingEvent("You are recruited to aid on a scientific mission."),
    (14 to 14) -> CharMod("In order to keep up with the stress of your responsibilities, you fall into bad habits.", TraitsNegativeEP.addictionModerate),
    (15 to 15) -> RandCharMods("You do the right thing but piss off someone with power in the process.", rand => List(RepNetworks.chooseAny(rand, 10), TraitsNegativeEP.enemy)),
    (16 to 16) -> Gear("You use the post-Fall chaos to establish a new identity.", UniqueGear("Fake Identity")),
    (17 to 17) -> MultipleEntries("After an unfortunate incident leads to lack (lost memories due to resleeving from an old backup), you decide security of mind is a worthy investment.", CharMod("", TraitsNegativeEP.editedMemories), Gear("", UniqueGear("Backup Insurance 1 year"))),
    (18 to 18) -> RandCharMod("After surviving the Fall and then only barely surviving a post-Fall clash, you decide that some self-defense training may be in order", Skills.modOnly(_, 20, SkillCategory.Combat)),
    (19 to 19) -> Gear("Through a strange set of circumstances, you end up with a rare Delphinium Six petal flower", UniqueGear("Delphinium Six petal flower")),
    (20 to 20) -> CharMod("You commit a crime, get caught, and suffer the punishment.", TraitsNegativeEP.modifiedBehaviourLevel3),
    (21 to 21) -> CharMod("You have died enough times that your mind really can’t take it any more.", TraitsNegativeTranshuman.phobiaDisorder.copy(name = "Phobia Disorder (Thanatophobia)")),
    (22 to 24) -> GatecrashingEvent("You save up and buy yourself a spot on a gatecrashing op."),
    (25 to 25) -> RandGear("An ownerless bot begins following you and never leaves your side.", Array(UniqueGear("Servitor Bot"), UniqueGear("Saucer Bot"), UniqueGear("Gnat Bot")).randomElement(_).get),
    (26 to 26) -> Gear("An unknown party leaves you a portable QE comm unit with a high-capacity reservoir (p. 315, EP), telling you only to expect a call in the future.", CommunicationsGear.portableQEComm), // reservoir?
    (27 to 27) -> RandMorph("You travel extensively.", ChoosingAMorph.randMorph),
    (28 to 28) -> MultipleEntries("You’ve gotten really good at this resleeving thing.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", TraitsPositiveEP.adaptabilityLevel1)),
    (29 to 29) -> CharMod("You narrowly avoid death in a disastrous accident.", TraitsPositiveEP.dangerSense),
    (30 to 30) -> CharMod("You barely survive a murder attempt.", Aptitude.INT + 5),
    (31 to 31) -> CharMod("A close-call with people out to get you puts you on the defensive.", TraitsPanopticon.informationControl),
    (32 to 32) -> CharMod("You’ve grown particular in your taste in morphs.", TraitsPositiveEP.rightAtHome),
    (33 to 33) -> CharMod("You fall on hard times.", StartingCredit - 10000),
    (34 to 34) -> RandMorph("You get caught in the cross-fire of a regional conflict.", ChoosingAMorph.randMorph),
    (35 to 35) -> CharMod("Your hectic lifestyle has increased your perceptive skills", TraitsPositiveEP.situationalAwareness),
    (36 to 36) -> CharMod("A long string of personal failures has you questioning your own resolve.", Aptitude.WIL - 5),
    (37 to 37) -> CharMod("No matter how often your friends warn you, you are promiscuous about your online data.", TraitsPanopticon.dataFootprint),
    (38 to 38) -> CharMod("Nothing ever seems to go your way—your cursed luck is legendary.", TraitsNegativeEP.badLuck),
    (39 to 39) -> RandCharMod("You complete a major project of importance to your work/faction.", RepNetworks.chooseAny(_, 10)),
    (40 to 40) -> RandCharMod("A project of importance to your work/faction fails under your direction.", RepNetworks.chooseAny(_, -10)),
    (41 to 41) -> RandCharMod("You take up arms in a regional conflict.", Skills.modOnly(_, 10, SkillCategory.Combat)),
    (42 to 42) -> RandMorph("Your work requires you to change your morph.", ChoosingAMorph.randMorph),
    (43 to 43) -> CharMod("You make an unpopular choice that burns many bridges.", TraitsNegativeEP.blacklistedOwn),
    (44 to 44) -> CharMod("A friend or relative opts for true death, but bequeaths you their estate.", StartingCredit + 25000),
    (45 to 45) -> CharMod("You are the unfortunate butt of a widespread online meme, but it works to your advantage.", TraitsPanopticon.fiveMinFame),
    (46 to 46) -> CharMod("You are the unfortunate butt of a widespread online meme, and it continues to haunt you.", TraitsPanopticon.fiveMinInfamy),
    (47 to 47) -> CharMod("As transhumanity re-organizes, you find a role in influencing others.", Aptitude.SAV + 5),
    (48 to 48) -> CharMod("You have difficulty coming to grips with regular resleeving.", TraitsNegativeEP.identityCrysis),
    (49 to 49) -> CharMod("One of your resleeves goes particularly poorly, and now it haunts you.", TraitsNegativeEP.morphingDisorderLevel2),
    (50 to 50) -> RandCharMod("You work hard to establish a solid network.", networking.modAnyField(_, +20)),
    (51 to 51) -> CharMod("You take up a sport.", Skills.oneOf(climbing, fray, freeFall, freerunning, swimming) + 20),
    (52 to 52) -> RandMorph("You decide to experiment.", ChoosingAMorph.randMorph),
    (53 to 53) -> RandCharMod("Practice makes perfect, and your hard work pays off.", StartingAge.AptitudeTables.any(+5).randomElement(_).get),
    (54 to 54) -> CharMod("You are the victim of an unfortunate crime.", StartingCredit - 10000),
    (55 to 55) -> CharMod("Someone steals your identity.", TraitsPanopticon.stolenIdentity),
    (56 to 56) -> CharMod("You play a prominent role in mediating a factional conflict.", protocol + 20),
    (57 to 57) -> CharMod("You spend a significant portion of your life in one habitat.", TraitsPanopticon.homeTurf),
    (58 to 58) -> MultipleEntries("You take a bullet for someone you don’t even know.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", Moxie + 1)),
    (59 to 59) -> CharMod("You piss off some powerful people and are made into an example.", TraitsNegativeEP.blackMarkLevel1),
    (60 to 60) -> CharMod("You make friends with a group of AGIs online.", TraitsPositiveTranshuman.agiAfinity),
    (61 to 61) -> CharMod("Due to a sudden financial crisis, you draw an emergency loan with unfavorable terms from an unforgiving loan shark.", TraitsNegativeTranshuman.debtLevel1),
    (62 to 62) -> CharMod("You commit a serious crime, but get away—for now.", TraitsNegativeEP.onTheRun),
    (63 to 65) -> GatecrashingEvent("You win the gatecrashing lottery and a free ticket to Pandora."),
    (66 to 66) -> CharMod("You go into business.", TraitsPositiveTranshuman.entrepreneurLevel1),
    (67 to 67) -> RandCharMod("You become embroiled in a messy professional dispute.", RepNetworks.chooseAny(_, -5)),
    (68 to 68) -> CharMod("You lose a contractual dispute in Extropian space.", TraitsNegativeTranshuman.defferedIndentureLevel1),
    (69 to 69) -> RandMorph("You rack up some debts and are forced to downgrade your lifestyle.", ChoosingAMorph.randMorph), // TODO cheap?!?
    (70 to 70) -> CharMod("You decide you need some help.", TraitsPositiveTranshuman.establishedFork),
    (71 to 71) -> CharMod("You run afoul of a criminal cartel agent. You walk away unscathed, but the matter is far from resolved.", TraitsNegativeEP.enemy),
    (72 to 72) -> RandCharMod("You get stuck with a boring, repetitive job, but at least you get really good at it.", Skills.specializeAny(_)),
    (73 to 73) -> CharMod("Doing your part to aid transhumanity’s regrowth, you have a kid.", TraitsNegativeTranshuman.dependent),
    (74 to 74) -> RandMorph("You have an unfortunately catastrophic sleeving accident, but the insurance paid well. Start play with 10 points of stress and a random derangement.", ChoosingAMorph.randMorph), // TODO fancy?
    (75 to 75) -> CharMod("You achieve something that the members of your faction will never forget.", TraitsPositiveTranshuman.goldStar),
    (76 to 76) -> RandCharMod("You score an impressive win in a public competition.", RepNetworks.chooseAny(_, +5)),
    (77 to 77) -> CharMod("You split off an alpha fork to handle an important situation, but it decides not to come back.", TraitsNegativeTranshuman.errantFork),
    (78 to 78) -> RandGear("You take in an abandoned animal.", Array(UniqueGear("Smart Dog"), UniqueGear("Smart Monkey"), UniqueGear("Smart Rat")).randomElement(_).get),
    (79 to 79) -> CharMod("You are the victim of a crime, but the perpetrator is caught. Now you hold their indenture contract.", TraitsPositiveTranshuman.indentureHolder),
    (80 to 80) -> CharMod("Everything is going great, but you still somehow manage to fuck up something major in your life.", Moxie + 1),
    (81 to 81) -> CharMod("A severe personal failure inspires you to make some radical changes.", TraitsNegativeEP.modifiedBehaviourLevel2),
    (82 to 82) -> CharMod("You find your one true love. You don’t feel like your full self when you are away from them.", TraitsNegativeTranshuman.intenseRelationship),
    (83 to 83) -> CharMod("You team up with a partner to get the job done.", TraitsPositiveTranshuman.minionPartner),
    (84 to 84) -> CharMod("Someone you respect shows their true colors, and they aren’t pretty.", Moxie + 1),
    (85 to 85) -> CharMod("Your exceptional nature is noticed.", TraitsPositiveEP.patron),
    (86 to 86) -> RandCharMod("You get fired/kicked out.", RepNetworks.chooseAny(_, -10)), // ignore OR
    (87 to 87) -> CharMod("An established university offers you a steady position.", TraitsPositiveTranshuman.tenure),
    (88 to 88) -> CharMod("A fork goes missing. It could be nothing, but it was in possession of some compromising information about yourself.", TraitsNegativeTranshuman.lostFork),
    (89 to 89) -> RandCharMod("You fall for the smooth lies of a convincing member of another faction. You realize your error only after the damage is done.", RepNetworks.chooseAny(_, -10)), // TODO faction filter?
    (90 to 90) -> MultipleEntries("A trolling hacker ruins your life but leaves you with sporting goodbye offering.", Gear("", UniqueGear("Kaos AI")), RandCharMod("", RepNetworks.chooseAny(_, -10))),
    (91 to 91) -> MultipleEntries("You are forced to resleeve in less-than-favorable conditions and end up with a morph with issues.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", TraitsNegativeTranshuman.aggresiveGrm)),
    (92 to 92) -> CharMod("You fall in with a new crowd—one that will have your back.", TraitsPositiveEP.allies),
    (93 to 93) -> CharMod("You are part of a group that discovers a derelict ship and makes a great salvaging score.", StartingCredit + 20000),
    (94 to 94) -> CharMod("You join a cooperative project.", TraitsPositiveTranshuman.entrepreneurLevel1),
    (95 to 95) -> CharMod("You fight back against a perceived injustice, but are forced to flee the repercussions.", TraitsNegativeEP.onTheRun),
    (96 to 96) -> RandCharMods("You drop everything to re-evaluate your priorities.", rand => List(Skills.modAny(rand, -30), Skills.modAny(rand, +50))), // TODO CP buys?
    (97 to 97) -> CharMod("Unknown to you, someone takes an unfriendly interest in your affairs.", TraitsNegativeTranshuman.subvertedMind),
    (98 to 98) -> MultipleEntries("You manage to get yourself killed three times in one week. At least you’re getting used to resleeving.", RandMorph("", ChoosingAMorph.randMorph), CharMod("", TraitsPositiveTranshuman.phoenixLevel2)),
    (99 to 99) -> CharMod("Someone leaves you in charge of their spacecraft.", TraitsPositiveTranshuman.spacecraft),
    (100 to 100) -> CharMod("You are complicit in a faction suffering a major setback.", TraitsNegativeEP.blackMarkLevel2));

  private val dataGatecrashing: RollTable[PostFallEventEntry] = RollTable(
    (1 to 20) -> CharMod("You go on 1d10 gatecrashing missions, with no major consequences. You do see some cool things, though.", Moxie + 1),
    (21 to 48) -> ExtraPackage("You serve on 1d10 missions and pick up some new skills.", FocusPackages.explorer.basicPackage),
    (49 to 50) -> Story("You make a new home on Portal (p. 122, Gatecrashing) or some other exoplanet outpost."),
    (51 to 52) -> RandMorph("You go through a gate but never come through on the other side. Start game with 10 points of stress.", ChoosingAMorph.randMorph),
    (53 to 54) -> CharMod("You receive some focused training in gate operations.", infosec.gainSpecialization("Gate Hacking")),
    (55 to 56) -> Story("You acquire an alien pet. You’re not allowed to bring it back to the solar system, however. Work with the gamemaster to determine the creature’s characteristics."),
    (57 to 58) -> CharMod("You find an alien artifact, but they didn’t let you keep it.", StartingCredit + 20000),
    (59 to 60) -> RandMorph("You discover a new xenocritter and its unique predatory capabilities.", ChoosingAMorph.randMorph),
    (61 to 62) -> RandMorph("Your mission backers upgrade/downgrade your capabilities.", ChoosingAMorph.randMorph),
    (63 to 64) -> CharMod("You participate in some eye-opening research.", Aptitude.COG + 5),
    (65 to 66) -> RandMorph("You discover left-behind TITAN machines.", ChoosingAMorph.randMorph),
    (67 to 68) -> RandMorph("You go on a mission and never return. Your sponsors refuse to talk about it.", ChoosingAMorph.randMorph),
    (69 to 70) -> CharMod("Your mission scores a major resource find.", StartingCredit + 20000),
    (71 to 72) -> CharMod("You receive some focused training in gate operations.", interfacing.gainSpecialization("Gate Operations")),
    (73 to 74) -> RandCharMod("You join a semi-successful colonization effort for a year", profession.modAnyField(_, +20)),
    (75 to 76) -> RandCharMod("You uncover evidence of a previously unknown but long-dead alien race.", RepNetworks.chooseAny(_, +10)),
    (77 to 78) -> RandCharMod("You put in several months of grueling work on a terraforming project.", rand => Skills.modOnly(rand, +10, academics, profession, SkillCategory.Technical)),
    (79 to 80) -> RandCharMod("You severely botch a rescue operation. Lives are lost and stacks are not recovered.", RepNetworks.chooseAny(_, -10)),
    (81 to 82) -> CharMod("You experience something while going through a gate that makes you never want to go through again.", TraitsNegativeTranshuman.phobiaDisorder.copy(name = "Phobia Disorder (Pandora Gates)")),
    (83 to 84) -> RandCharMod("You participate in a dangerous rescue operation.", RepNetworks.chooseAny(_, +10)),
    (85 to 86) -> CharMod("You receive some focused training in gate operations.", academics.withField("Gate Operations") + 20),
    (87 to 88) -> Gear("You come into possession of your very own blue box (p. 157, Gatecrashing).", UniqueGear("Blue Box")),
    (89 to 90) -> CharMod("You survive a lengthy gatehopping adventure.", RepNetworks.xRep + 20),
    (91 to 92) -> RandMorph("Your mission is sabotaged by an unknown party.", ChoosingAMorph.randMorph),
    (93 to 94) -> CharMod("You step through a gate and arrive somewhere other than you expected. Your jaunt is adventurous, but you make it back safe.", profession.withField("Gatecrashing") + 10),
    (95 to 96) -> CharMod("You receive some focused training in gate operations.", programming.gainSpecialization("Gate Interface")),
    (97 to 98) -> RandMorph("You end up in the middle of an exoplanet property-claim dispute.", ChoosingAMorph.randMorph),
    (99 to 100) -> CharMod("You participate in a rescue op that cashes in on a sizable recovery bond.", StartingCredit + 10000));

  override def label: String = "Post-Fall Life Event";
  override def source: String = "Transhuman p.67-69";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get.resolve(rand)
  }
}

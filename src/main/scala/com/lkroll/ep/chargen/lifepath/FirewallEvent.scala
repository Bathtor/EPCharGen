package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Aptitude, Cost, Effect, EPTrait, MorphModel, MorphType, Gear => DataGear }
import com.lkroll.ep.compendium.data._

case class FirewallEventResult(descr: String, effects: List[FirewallEventEffect]) {
  def effectList: List[String] = effects.map(_.render);
}
sealed trait FirewallEventEffect {
  def render: String;
}
object FirewallEventEffect {
  case class CharMod(mod: CharacterMod) extends FirewallEventEffect {
    override def render: String = mod.render;
  }
  case class NewMorph(morph: MorphModel) extends FirewallEventEffect {
    override def render: String = s"Changed Morph to ${morph.name}";
  }
}

object FirewallEvent extends Table {
  import Implicits.RandomArray
  import CharImplicits._;
  import DefaultSkills._;
  import PackageImplicits.{ Moxie, StartingCredit, morphT2filter };

  override type Result = FirewallEventResult;

  sealed trait FirewallEventEntry {
    def resolve(rand: Random): FirewallEventResult;
  }
  case class CharMod(descr: String, mod: CharacterMod) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      FirewallEventResult(descr, List(FirewallEventEffect.CharMod(mod)))
    }
  }
  case class RandCharMod(descr: String, mod: Random => CharacterMod) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      FirewallEventResult(descr, List(FirewallEventEffect.CharMod(mod(rand))))
    }
  }
  case class CharMods(descr: String, mods: List[CharacterMod]) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      val effects = mods.map(mod => FirewallEventEffect.CharMod(mod));
      FirewallEventResult(descr, effects)
    }
  }
  case class RandCharMods(descr: String, mods: Random => List[CharacterMod]) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      val effects = mods(rand).map(mod => FirewallEventEffect.CharMod(mod));
      FirewallEventResult(descr, effects)
    }
  }
  case class RandMorph(descr: String, morph: Random => MorphModel) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      FirewallEventResult(descr, List(FirewallEventEffect.NewMorph(morph(rand))))
    }
  }
  case class MultipleEntries(descr: String, entries: FirewallEventEntry*) extends FirewallEventEntry {
    override def resolve(rand: Random): FirewallEventResult = {
      val effects = entries.foldLeft(List.empty[FirewallEventEffect]) { (acc, entry) =>
        entry.resolve(rand).effects ::: acc
      };
      FirewallEventResult(descr, effects)
    }
  }

  private val data: RollTable[FirewallEventEntry] = RollTable(
    (1 to 25) -> RandCharMod("You are recruited by someone you know because of your skill sets.", Skills.modAny(_, +10)),
    (26 to 48) -> CharMod("You accidentally stumble onto a Firewall op, and luckily for you they decide the best option is to recruit you.", Moxie + 1),
    (49 to 50) -> CharMod("You work with/for someone who turns out to be an exhuman supporter. Once you get over the shock, Firewall recruits you.", interest.withField("Exhumans") + 10),
    (51 to 52) -> CharMod("Firewall recruits you as an informant, to help keep tabs on someone or something they are worried about.", infosec + 10),
    (53 to 54) -> CharMod("Your ego was jailed/lost on Earth/forknapped/in cold storage, but Firewall broke you out in return for your aid.", Moxie + 1),
    (55 to 56) -> CharMod("You spot someone acting suspiciously and report them. They turn out to be a Firewall async. You are repaid with recruitment and training.", TraitsPositiveTranshuman.asyncFamiliarity),
    (57 to 58) -> MultipleEntries("You are infected and secretly operate as a sleeper exsurgent for months or even years. Firewall restores you from an old backup.", CharMod("", TraitsNegativeEP.editedMemories), RandMorph("", ChoosingAMorph.randMorph)),
    (59 to 60) -> RandMorph("You have an unexpected close encounter with the TQZ on Mars, the New Mumbai Containment Zone of Luna, or Iapetus.", ChoosingAMorph.randMorph),
    (61 to 62) -> RandMorph("An exhuman raid leaves you and others dead; Firewall helps sort out the mess.", ChoosingAMorph.randMorph),
    (63 to 64) -> CharMod("You are one of the few survivors of an exsurgent outbreak on your habitat.", TraitsPositiveEP.psiDefense1),
    (65 to 66) -> RandCharMod("You find a relic. Bad things happen. Firewall cleans up the mess.", RepNetworks.chooseAny(_, -10)),
    (67 to 68) -> CharMod("You were a member/supporter of one of the groups that evolved into Firewall from before the Fall. You took some time off, but now you’re back.", RepNetworks.iRep + 10),
    (69 to 70) -> CharMod("You single-handedly foil an impending outbreak, but the local authorities blame you for the carnage. Firewall helps you escape.", TraitsNegativeEP.onTheRun),
    (71 to 72) -> RandMorph("A previously dormant TITAN nanoplague rampages through your habitat. ", ChoosingAMorph.randMorph),
    (73 to 74) -> RandMorph("Your ship stops to investigate/help a derelict ship and is never heard from again.", ChoosingAMorph.randMorph),
    (75 to 76) -> RandCharMod("You discover a lost cache on an isolated asteroid. Firewall actually lets you keep some of the find.", StartingCredit + (Dice.`1d10` * 5000).randomElement(_).get),
    (77 to 78) -> CharMod("You step into an unknown fray and are lucky enough to pick the right side.", RepNetworks.iRep + 10),
    (79 to 80) -> RandCharMod("You uncover a conspiracy within your faction and Firewall steps in to help you deal with it.", RepNetworks.chooseAny(_, +10)),
    (81 to 82) -> RandCharMods("You are recruited to help Firewall cover up a secret or outbreak.", rand => List(RepNetworks.chooseAny(rand, -10), RepNetworks.iRep + 10)),
    (83 to 84) -> CharMod("You uncover a sleeper exsurgent cell the hard way.", TraitsNegativeTranshuman.phobiaDisorder.copy(name = "Phobia Disorder (Exsurgents)")),
    (85 to 86) -> CharMod("You become aware of someone smuggling or dealing TITAN technology. Firewall steps in and deals with it, then recruits you.", RepNetworks.gRep - 5),
    (87 to 88) -> CharMod("You were a member/supporter of one of the groups that evolved into Firewall from before the Fall.", RepNetworks.iRep + 10),
    (89 to 90) -> CharMod("You are involved in a cover-up of a TITAN- or exsurgent-related secret during the Fall. Firewall finds you and brings you in to get the story.", RepNetworks.iRep + 10),
    (91 to 92) -> CharMod("Someone you loved becomes infected. You keep it secret and protect them for a time, until everything goes bad. Firewall rescues you, then recruits you.", interest.withField("Exsurgents") + 10),
    (93 to 94) -> CharMod("You are a bit too good at ferreting out certain secrets online. Firewall brings you in to the fold to keep your mouth shut.", infosec + 10),
    (95 to 96) -> CharMod("Someone you are close to is a Firewall proxy; they brought you in to help them out.", Moxie + 1),
    (97 to 98) -> CharMod("Thanks to a particular skill you have, Firewall has consulted with you for years without revealing themselves. They decide to fill you in on the full story.", academics.withField("Nanotechnology") + 30), // not quite what it says, but eh
    (99 to 100) -> CharMod("A package you are hired to deliver turns out to be an alien artifact. When it causes problems, you go to a friend of a friend, who turns out to be a Firewall proxy. They solve the problem, but you fail to make the delivery.", TraitsNegativeEP.enemy));
  override def label: String = "Firewall Event";
  override def source: String = "Transhuman p.71";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get.resolve(rand)
  }
}

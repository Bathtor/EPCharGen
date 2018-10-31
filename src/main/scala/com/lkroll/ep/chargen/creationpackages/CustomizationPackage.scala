package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.Random
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium.{ Aptitude, SkillCategory }
import com.lkroll.ep.compendium.data.{ DefaultSkills, Disorders => DisorderData, _ }

case class CustomizationPackage(
  label:  String,
  mods:   List[PackageContent]       = Nil,
  skills: List[PackageContent.Skill] = Nil) extends PPPackage {
  override def ppCost: Int = 1;
  override def applyTo(c: CharGenCharacter, rand: Random): CharGenCharacter = {
    val moddedChar = mods.foldLeft(c) { (acc, mod) =>
      mod match {
        case PackageContent.Mod(m)       => m.applyTo(acc)
        case PackageContent.Mods(ms)     => ms.foldLeft(acc)((acc2, m) => m.applyTo(acc2))
        case PackageContent.RandMod(mf)  => mf(rand).applyTo(acc)
        case PackageContent.RandMods(ms) => ms(rand).foldLeft(acc)((acc2, m) => m.applyTo(acc2))
      }
    };
    val concreteSkills = skills.map(_.toSkill(rand));
    moddedChar.copy(skills = moddedChar.skills ++ concreteSkills)
  }
}

object CustomizationPackages {
  import PackageImplicits._;
  import DefaultSkills.{ list => skillList, _ };
  import CharImplicits.{ skillcls2filter, skillcat2filter, string2filter, skill2filter, skilldef2skill };
  import RepNetworks._;

  val artist = CustomizationPackage(
    label = "Artist",
    skills = List(
      art.anyField(40),
      disguise.at(30),
      interest.anyField(30)));

  val async = CustomizationPackage(
    label = "Async",
    mods = List(
      TraitsPositiveEP.psi1,
      rpc(Disorders.roll(_)),
      rpc(rand => if (rand.nextBoolean()) TraitsPositiveEP.psiChameleon else TraitsPositiveEP.psiDefense1),
      CharacterMod.BecomeAsync,
      Sleights.PsiChi + 5),
    skills = List(
      interest.anyField(45)));

  val asyncAdept = CustomizationPackage(
    label = "Async Adept",
    mods = List(
      TraitsPositiveEP.psi2,
      rpc(Disorders.roll(_)),
      rpc(Disorders.roll(_)),
      CharacterMod.BecomeAsync,
      Sleights.PsiChi + 7),
    skills = List(
      Skills.chooseOnly(40, SkillCategory.Psi)));

  val athletics = CustomizationPackage(
    label = "Athletics",
    skills = List(
      climbing.at(30),
      freerunning.at(40),
      swimming.at(30)));

  val computerTraining = CustomizationPackage(
    label = "Computer Training",
    skills = List(
      infosec.at(40),
      interfacing.at(30),
      programming.at(30)));

  val connected = CustomizationPackage(
    label = "Connected",
    mods = List(
      StartingCredit + 30000,
      rpc(rand => if (rand.nextBoolean()) TraitsPositiveEP.allies else TraitsPositiveEP.patron)),
    skills = List(
      networking.anyField(40)));

  val essentialSkills = CustomizationPackage(
    label = "Essential Skills",
    skills = List(
      fray.at(30),
      networking.anyField(30),
      perception.at(40)));

  val gearhead = CustomizationPackage(
    label = "Gearhead",
    mods = List(StartingCredit + 60000),
    skills = List(
      hardware.anyField(40)));

  val heavyWeaponsTraining = CustomizationPackage(
    label = "Heavy Weapons Training",
    skills = List(
      demolitions.at(30),
      gunnery.at(30),
      seekerWeapons.at(40)));

  val jackOfAllTrades = CustomizationPackage(
    label = "Jack-of-All-Trades",
    skills = List(
      Skills.chooseAny(40),
      Skills.chooseAny(30),
      Skills.chooseAny(30)));

  val lucky = CustomizationPackage(
    label = "Lucky",
    mods = List(Moxie + 3),
    skills = List(
      Skills.chooseAny(30),
      Skills.chooseAny(25)));

  val martialArtsTraining = CustomizationPackage(
    label = "Martial Arts Training",
    skills = List(
      blades.at(30),
      throwingWeapons.at(30),
      unarmedCombat.at(40)));

  val mentalist = CustomizationPackage(
    label = "Mentalist",
    mods = List(Sleights + 4),
    skills = List(
      Skills.chooseOnly(40, SkillCategory.Psi),
      Skills.chooseOnly(40, SkillCategory.Psi)));

  val networker = CustomizationPackage(
    label = "Networker",
    mods = List(r(RepNetworks.chooseAny(_, +100))),
    skills = List(
      networking.anyField(30),
      networking.anyField(30),
      networking.anyField(30)));

  val paramedic = CustomizationPackage(
    label = "Paramedic",
    skills = List(
      medicine.withField("Paramedic").at(40),
      medicine.withField("Nanomedicine").at(30),
      programming.at(30)));

  val slacker = CustomizationPackage(
    label = "Slacker",
    skills = List(
      interest.anyField(40),
      interest.anyField(30),
      scrounging.at(30)));

  val sneaker = CustomizationPackage(
    label = "Sneaker",
    skills = List(
      disguise.at(40),
      impersonation.at(30),
      infiltration.at(30)));

  val socialButterfly = CustomizationPackage(
    label = "Social Butterfly",
    skills = List(
      deception.at(30),
      persuasion.at(40),
      protocol.at(30)));

  val spacer = CustomizationPackage(
    label = "Spacer",
    skills = List(
      freeFall.at(40),
      kinesics.at(30),
      medicine.anyField(30)));

  val student = CustomizationPackage(
    label = "Student",
    skills = List(
      academics.anyField(40),
      interest.anyField(30),
      research.at(30)));

  val survivalTraining = CustomizationPackage(
    label = "Survival Training",
    skills = List(
      kineticWeapons.at(30),
      medicine.withField("Paramedic").at(30),
      scrounging.at(40)));

  val techTraining = CustomizationPackage(
    label = "Tech Training",
    skills = List(
      hardware.anyField(40),
      hardware.anyField(30),
      programming.at(30)));

  val weaponsTraining = CustomizationPackage(
    label = "Weapons Training",
    skills = List(
      beamWeapons.at(30),
      kineticWeapons.at(40),
      sprayWeapons.at(30)));
}

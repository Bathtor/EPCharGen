package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.Random
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium.Aptitude
import com.lkroll.ep.compendium.data.{ Disorders => DisorderData, _ }

case class FocusPackage(
  label:       String,
  level:       PackageLevel,
  motivations: List[Motivation]           = Nil,
  mods:        List[PackageContent]       = Nil,
  skills:      List[PackageContent.Skill] = Nil) extends Package {
  override type Self = FocusPackage;
  override def withPrefix(prefix: String): Self = this.copy(label = s"$prefix $label");
  override def ppCost: Int = level.ppCost;
  override def applyTo(c: Character, rand: Random): Character = {
    val motivatedChar = c.copy(motivations = c.motivations ++ motivations);
    val moddedChar = mods.foldLeft(motivatedChar) { (acc, mod) =>
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

object FocusPackages {
  import PackageImplicits._;
  import Skills.Defaults.{ list => skillList, _ }
  import CharImplicits.{ skillcls2filter, skillcat2filter, string2filter, skill2filter, string2motivation };
  import RepNetwork._;

  val academic = PackageGroup(
    label = "Academic",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Education", "+Open Source", "+Personal Career", "+Personal Development"),
      mods = Nil,
      skills = List(
        academics.anyField(40),
        academics.anyField(30),
        research.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Education", "+Open Source", "+Personal Career", "+Personal Development"),
      mods = List(RepNetwork.rRep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        academics.anyField(40),
        investigation.at(30),
        language.anyField(30),
        networking.withField("Scientists").at(35),
        perception.at(20),
        profession.withField("Instruction").at(20),
        research.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Education", "+Open Source", "+Personal Career", "+Personal Development"),
      mods = List(Aptitude.COG + 5, RepNetwork.rRep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        academics.anyField(40),
        investigation.at(50),
        language.anyField(30),
        networking.anyField(30),
        networking.withField("Scientists").at(50),
        perception.at(30),
        persuasion.at(30),
        profession.withField("Instruction").at(50),
        research.at(45))));

  val activist = PackageGroup(
    label = "Activist",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Mercurial Cause", "+Privacy", "+Terraforming", "+Venusian Sovereignty"),
      mods = Nil,
      skills = List(
        interest.anyField(30),
        persuasion.at(40),
        research.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Mercurial Cause", "+Privacy", "+Terraforming", "+Venusian Sovereignty"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        fray.at(20),
        infosec.at(20),
        interest.anyField(50),
        investigation.at(20),
        language.anyField(20),
        networking.anyField(30),
        perception.at(20),
        persuasion.at(40),
        research.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Bioconservatism", "+Mercurial Cause", "+Privacy", "+Terraforming", "+Venusian Sovereignty"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        fray.at(20),
        infiltration.at(30),
        infosec.at(20),
        interest.anyField(50),
        interest.anyField(20),
        investigation.at(30),
        language.anyField(30),
        networking.anyField(40),
        perception.at(20),
        persuasion.at(50),
        protocol.at(30),
        research.at(340),
        unarmedCombat.at(20))));

  val assassin = PackageGroup(
    label = "Assassin",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Personal Career", "+Privacy", "+Survival"),
      mods = Nil,
      skills = List(
        kineticWeapons.at(40),
        profession.withField("Assassin").at(30),
        unarmedCombat.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Personal Career", "+Privacy", "+Survival"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        blades.at(25),
        fray.at(30),
        infiltration.at(40),
        kineticWeapons.at(50),
        language.anyField(40),
        perception.at(20),
        profession.withField("Assassin").at(50),
        unarmedCombat.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Personal Career", "+Privacy", "+Survival"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.anyField(30),
        blades.at(35),
        disguise.at(40),
        freerunning.at(30),
        fray.at(30),
        infiltration.at(50),
        interest.anyField(30),
        kineticWeapons.at(50),
        language.anyField(40),
        perception.at(30),
        profession.withField("Assassin").at(50),
        sprayWeapons.at(30),
        unarmedCombat.at(50))));

  val bodyguard = PackageGroup(
    label = "Bodyguard",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Duty", "+Privacy", "+Survival"),
      mods = Nil,
      skills = List(
        kinesics.at(40),
        profession.withField("Bodyguard").at(30),
        unarmedCombat.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Duty", "+Privacy", "+Survival"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        beamWeapons.at(30),
        fray.at(30),
        intimidation.at(30),
        kinesics.at(40),
        language.anyField(40),
        perception.at(40),
        profession.withField("Bodyguard").at(50),
        unarmedCombat.at(35))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Duty", "+Privacy", "+Survival"),
      mods = List(
        Aptitude.REF + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        beamWeapons.at(30),
        fray.at(30),
        interest.anyField(30),
        interest.anyField(30),
        intimidation.at(30),
        kinesics.at(50),
        kineticWeapons.at(30),
        language.anyField(40),
        medicine.withField("Paramedic").at(25),
        perception.at(50),
        profession.withField("Bodyguard").at(50),
        unarmedCombat.at(50))));

  val botJammer = PackageGroup(
    label = "Bot Jammer",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+DIY", "+Maker Movement", "+Thrill Seeking"),
      mods = Nil,
      skills = List(
        hardware.anyField(30),
        interest.anyField(30),
        pilot.anyField(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+DIY", "+Maker Movement", "+Thrill Seeking"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        flight.at(40),
        hardware.anyField(40),
        interest.anyField(30),
        interest.anyField(30),
        perception.at(20),
        pilot.anyField(50),
        pilot.anyField(30),
        seekerWeapons.at(25))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+DIY", "+Maker Movement", "+Thrill Seeking"),
      mods = List(
        Aptitude.REF + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        flight.at(50),
        fray.at(20),
        hardware.anyField(40),
        interest.anyField(40),
        interest.anyField(40),
        navigation.at(20),
        perception.at(30),
        pilot.anyField(50),
        pilot.anyField(30),
        profession.anyField(40),
        seekerWeapons.at(25))));

  val combatAsync = PackageGroup(
    label = "Combat Async",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Neurodiversity", "+Self Control", "+Survival", "–Bloodlust"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 3),
      skills = List(
        profession.withField("Squad Tactics").at(30),
        psiAssault.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Neurodiversity", "+Self Control", "+Survival", "–Bloodlust"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 4,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        fray.at(20),
        infiltration.at(30),
        interest.anyField(40),
        perception.at(20),
        profession.withField("Squad Tactics").at(50),
        psiAssault.at(50),
        sprayWeapons.at(20),
        unarmedCombat.at(20))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Neurodiversity", "+Self Control", "+Survival", "–Bloodlust"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 8,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        control.at(30),
        fray.at(30),
        infiltration.at(40),
        interest.anyField(40),
        language.anyField(30),
        perception.at(30),
        profession.withField("Squad Tactics").at(50),
        psiAssault.at(50),
        sense.at(30),
        sprayWeapons.at(40),
        unarmedCombat.at(30))));

  val conArtist = PackageGroup(
    label = "Con Artist",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Privacy", "+Survival", "+Vice"),
      mods = Nil,
      skills = List(
        deception.at(40),
        profession.withField("Con Schemes").at(30),
        persuasion.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Privacy", "+Survival", "+Vice"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        deception.at(50),
        disguise.at(30),
        impersonation.at(30),
        kinesics.at(15),
        language.anyField(40),
        palming.at(20),
        perception.at(20),
        profession.withField("Con Schemes").at(50),
        persuasion.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Privacy", "+Survival", "+Vice"),
      mods = List(
        Aptitude.SAV + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(40),
        deception.at(50),
        disguise.at(40),
        impersonation.at(40),
        infosec.at(30),
        interest.anyField(30),
        kinesics.at(25),
        language.anyField(30),
        perception.at(20),
        palming.at(40),
        profession.withField("Con Schemes").at(50),
        persuasion.at(50))));

  val controllerAsync = PackageGroup(
    label = "Controller Async",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Neurodiversity", "+Self Control"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 3),
      skills = List(
        academics.withField("Psychology").at(30),
        control.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Neurodiversity", "+Self Control"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 4,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(40),
        control.at(50),
        deception.at(25),
        perception.at(30),
        persuasion.at(35),
        profession.withField("Psychotherapy").at(50),
        psychosurgery.at(20))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Exploration", "+Neurodiversity", "+Self Control"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 6,
        Aptitude.WIL + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(40),
        control.at(50),
        deception.at(40),
        intimidation.at(25),
        interest.anyField(30),
        language.anyField(30),
        perception.at(30),
        persuasion.at(45),
        profession.withField("Psychotherapy").at(50),
        psychosurgery.at(25),
        sense.at(25))));

  val covertOps = PackageGroup(
    label = "Covert Ops",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Explosions", "+Survival", "–TITAN Tech"),
      mods = Nil,
      skills = List(
        blades.at(30),
        infiltration.at(40),
        profession.withField("Squad Tactics").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Explosions", "+Survival", "–TITAN Tech"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        blades.at(20),
        demolitions.at(25),
        fray.at(20),
        infiltration.at(50),
        kineticWeapons.at(40),
        language.anyField(40),
        perception.at(20),
        profession.withField("Squad Tactics").at(50),
        unarmedCombat.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Explosions", "+Survival", "–TITAN Tech"),
      mods = List(
        Aptitude.COO + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        blades.at(30),
        climbing.at(30),
        demolitions.at(30),
        fray.at(30),
        infiltration.at(50),
        infosec.at(25),
        interest.anyField(30),
        interest.anyField(30),
        kineticWeapons.at(40),
        language.anyField(40),
        perception.at(20),
        profession.withField("Squad Tactics").at(50),
        unarmedCombat.at(40))));

  val dealer = PackageGroup(
    label = "Dealer",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Hypercapitalism", "+Personal Career"),
      mods = Nil,
      skills = List(
        kinesics.at(30),
        profession.anyField(30),
        persuasion.at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Hypercapitalism", "+Personal Career"),
      mods = List(r(RepNetwork.chooseAny(_, 50))),
      skills = List(
        beamWeapons.at(20),
        deception.at(25),
        interest.anyField(40),
        kinesics.at(40),
        networking.anyField(40),
        profession.withField("Haggling").at(50),
        perception.at(30),
        persuasion.at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Hard Work", "+Hypercapitalism", "+Personal Career"),
      mods = List(
        r(RepNetwork.chooseAny(_, 100)),
        Aptitude.SAV + 5),
      skills = List(
        academics.withField("Economics").at(30),
        beamWeapons.at(20),
        deception.at(25),
        fray.at(15),
        interest.anyField(30),
        kinesics.at(40),
        networking.anyField(50),
        networking.anyField(40),
        networking.anyField(20),
        perception.at(30),
        profession.withField("Haggling").at(50),
        profession.withField("Social Engineering").at(40),
        persuasion.at(50))));

  val egoHunter = PackageGroup(
    label = "Ego Hunter",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Justice", "+Self Reliance", "+The Hunt"),
      mods = Nil,
      skills = List(
        investigation.at(30),
        kinesics.at(40),
        profession.withField("Skip Tracing").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Justice", "+Self Reliance", "+The Hunt"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        infosec.at(15),
        investigation.at(30),
        kinesics.at(50),
        language.anyField(40),
        perception.at(15),
        profession.withField("Skip Tracing").at(50),
        research.at(40),
        sprayWeapons.at(25),
        unarmedCombat.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Justice", "+Self Reliance", "+The Hunt"),
      mods = List(
        Aptitude.INT + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        fray.at(25),
        infosec.at(30),
        interest.anyField(30),
        interest.anyField(30),
        investigation.at(40),
        kinesics.at(50),
        language.anyField(40),
        networking.anyField(20),
        perception.at(20),
        profession.withField("Skip Tracing").at(50),
        research.at(50),
        sprayWeapons.at(30),
        unarmedCombat.at(30))));

  val enforcer = PackageGroup(
    label = "Enforcer",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Cartel Growth", "+Fascism", "+Self Reliance", "+Stability"),
      mods = Nil,
      skills = List(
        intimidation.at(40),
        profession.withField("Enforcement").at(30),
        unarmedCombat.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Cartel Growth", "+Fascism", "+Self Reliance", "+Stability"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        clubs.at(30),
        fray.at(20),
        freerunning.at(30),
        intimidation.at(50),
        language.anyField(40),
        perception.at(20),
        profession.withField("Enforcement").at(50),
        sprayWeapons.at(25),
        unarmedCombat.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Cartel Growth", "+Fascism", "+Self Reliance", "+Stability"),
      mods = List(
        Aptitude.SOM + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        clubs.at(30),
        fray.at(20),
        freerunning.at(30),
        interest.anyField(40),
        interest.anyField(40),
        intimidation.at(50),
        language.anyField(40),
        networking.anyField(30),
        palming.at(20),
        perception.at(20),
        profession.withField("Enforcement").at(50),
        sprayWeapons.at(30),
        unarmedCombat.at(45))));

  val explorer = PackageGroup(
    label = "Explorer",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Alien Contact", "+Education", "+Exploration", "+Survival"),
      mods = Nil,
      skills = List(
        freerunning.at(40),
        investigation.at(30),
        profession.withField("Gatecrashing").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Alien Contact", "+Education", "+Exploration", "+Survival"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        climbing.at(30),
        fray.at(25),
        freerunning.at(35),
        investigation.at(20),
        perception.at(20),
        profession.withField("Gatecrashing").at(50),
        scrounging.at(40),
        swimming.at(20))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Alien Contact", "+Education", "+Exploration", "+Survival"),
      mods = List(
        Moxie + 1,
        Aptitude.SOM + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        climbing.at(40),
        fray.at(25),
        freerunning.at(40),
        infiltration.at(25),
        interest.anyField(30),
        investigation.at(20),
        kineticWeapons.at(30),
        perception.at(30),
        profession.withField("Gatecrashing").at(50),
        profession.anyField(30),
        scrounging.at(40),
        swimming.at(30))));

  val face = PackageGroup(
    label = "Face",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = Nil,
      skills = List(
        kinesics.at(30),
        profession.withField("Social Engineering").at(30),
        persuasion.at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        deception.at(35),
        kinesics.at(30),
        language.anyField(40),
        networking.anyField(40),
        perception.at(20),
        profession.withField("Social Engineering").at(50),
        persuasion.at(50),
        protocol.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(30),
        deception.at(40),
        fray.at(15),
        interest.anyField(30),
        kinesics.at(40),
        language.anyField(40),
        networking.anyField(40),
        perception.at(20),
        profession.withField("Social Engineering").at(50),
        persuasion.at(50),
        protocol.at(35))));

  val genehacker = PackageGroup(
    label = "Genehacker",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Artistic Expression", "+Morphological Freedom", "+Research", "+Science!", "+Uplift Rights"),
      mods = Nil,
      skills = List(
        academics.withField("Genetics").at(30),
        medicine.anyField(40),
        medicine.anyField(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Artistic Expression", "+Morphological Freedom", "+Research", "+Science!", "+Uplift Rights"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Genetics").at(50),
        academics.anyField(40),
        animalHandling.at(20),
        medicine.anyField(50),
        medicine.anyField(40),
        networking.withField("Scientists").at(20),
        perception.at(20),
        profession.withField("Lab Tech").at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Artistic Expression", "+Morphological Freedom", "+Research", "+Science!", "+Uplift Rights"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        RepNetwork.rRep + 50),
      skills = List(
        academics.withField("Genetics").at(50),
        academics.anyField(40),
        animalHandling.at(40),
        interest.anyField(30),
        interfacing.at(20),
        investigation.at(30),
        medicine.anyField(50),
        medicine.anyField(40),
        medicine.anyField(40),
        networking.withField("Scientists").at(20),
        perception.at(30),
        profession.withField("Lab Tech").at(40))));

  val hacker = PackageGroup(
    label = "Hacker",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Fame", "+Open Source", "+Owning Systems", "+Thrill Seeking", "–Hackers"),
      mods = Nil,
      skills = List(
        infosec.at(40),
        profession.withField("Mesh Security Ops").at(30),
        programming.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Fame", "+Open Source", "+Owning Systems", "+Thrill Seeking", "–Hackers"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.oneOf("Computer Science", "Cryptography").at(40),
        hardware.withField("Electronics").at(15),
        infosec.at(50),
        interfacing.at(30),
        networking.withField("Criminal").at(20),
        perception.at(20),
        profession.withField("Mesh Security Ops").at(50),
        programming.at(40),
        research.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Fame", "+Open Source", "+Owning Systems", "+Thrill Seeking", "–Hackers"),
      mods = List(
        Aptitude.COG + 5,
        RepNetwork.gRep + 50),
      skills = List(
        academics.withField("Computer Science").at(40),
        academics.withField("Cryptography").at(40),
        fray.at(10),
        hardware.withField("Electronics").at(25),
        infiltration.at(20),
        infosec.at(50),
        interfacing.at(50),
        networking.withField("Criminal").at(30),
        perception.at(20),
        profession.withField("Mesh Security Ops").at(40),
        profession.withField("Social Engineering").at(30),
        programming.at(50),
        research.at(40))));

  val icon = PackageGroup(
    label = "Icon",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Art", "+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = Nil,
      skills = List(
        art.anyField(40),
        kinesics.at(30),
        protocol.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Art", "+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.anyField(50),
        disguise.at(30),
        impersonation.at(20),
        interest.anyField(40),
        kinesics.at(45),
        perception.at(30),
        persuasion.at(40),
        protocol.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Art", "+Fame", "+Hedonism", "+Personal Career", "+Thrill Seeking"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        RepNetwork.fRep + 50,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.anyField(50),
        art.anyField(30),
        disguise.at(30),
        fray.at(10),
        impersonation.at(25),
        interest.anyField(40),
        kinesics.at(45),
        networking.anyField(25),
        networking.withField("Media").at(30), // book says Socialites, but I don't want to split skill points even more
        perception.at(20),
        persuasion.at(40),
        profession.anyField(30),
        protocol.at(40))));

  val investigator = PackageGroup(
    label = "Investigator",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Self Reliance", "+The Hunt"),
      mods = Nil,
      skills = List(
        investigation.at(40),
        profession.withField("Forensics").at(30),
        research.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Self Reliance", "+The Hunt"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        infosec.at(40),
        investigation.at(50),
        kinesics.at(30),
        kineticWeapons.at(20),
        perception.at(25),
        profession.withField("Forensics").at(50),
        profession.withField("Police Procedures").at(40),
        research.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Self Reliance", "+The Hunt"),
      mods = List(
        Aptitude.INT + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        fray.at(20),
        infosec.at(30),
        interest.anyField(30),
        intimidation.at(30),
        investigation.at(50),
        kinesics.at(30),
        kineticWeapons.at(30),
        language.anyField(30),
        networking.anyField(20),
        perception.at(20),
        profession.withField("Forensics").at(50),
        profession.withField("Police Procedures").at(40),
        research.at(40),
        unarmedCombat.at(25))));

  val journo = PackageGroup(
    label = "Journo",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Sousveillance", "+Transparency", "–Censorship"),
      mods = Nil,
      skills = List(
        investigation.at(30),
        persuasion.at(40),
        profession.withField("Journalism").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Sousveillance", "+Transparency", "–Censorship"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.withField("Performance").at(40),
        intimidation.at(20),
        investigation.at(40),
        networking.anyField(20),
        persuasion.at(50),
        perception.at(35),
        profession.withField("Journalism").at(50),
        research.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Sousveillance", "+Transparency", "–Censorship"),
      mods = List(
        Aptitude.SAV + 5,
        r(RepNetwork.chooseAny(_, +100))),
      skills = List(
        academics.anyField(30),
        art.withField("Performance").at(40),
        disguise.at(25),
        fray.at(20),
        interest.anyField(30),
        intimidation.at(30),
        investigation.at(40),
        kinesics.at(20),
        networking.anyField(40),
        perception.at(25),
        persuasion.at(50),
        profession.withField("Journalism").at(50),
        research.at(40))));

  val medic = PackageGroup(
    label = "Medic",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Helping Others", "–Violence"),
      mods = Nil,
      skills = List(
        medicine.withField("Paramedic").at(40),
        medicine.anyField(30),
        profession.withField("Medical Care").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Helping Others", "–Violence"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        hardware.withField("Implants").at(25),
        medicine.withField("Paramedic").at(50),
        medicine.anyField(40),
        networking.anyField(20),
        perception.at(35),
        persuasion.at(20),
        profession.withField("Medical Care").at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Helping Others", "–Violence"),
      mods = List(
        Moxie + 2,
        Aptitude.COG + 5,
        RepNetwork.rRep + 50),
      skills = List(
        academics.anyField(40),
        academics.anyField(30),
        fray.at(30),
        hardware.withField("Implants").at(20),
        interest.anyField(30),
        interfacing.at(15),
        medicine.withField("Paramedic").at(50),
        medicine.anyField(40),
        networking.anyField(20),
        perception.at(30),
        persuasion.at(20),
        profession.withField("Medical Care").at(50))));

  val pirate = PackageGroup(
    label = "Pirate",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Survival", "+Thrill Seeking", "+Wealth", "–Authority"),
      mods = List(Moxie + 1),
      skills = List(
        infiltration.at(15),
        interest.anyField(30),
        pilot.withField("Spacecraft").at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Survival", "+Thrill Seeking", "+Wealth", "–Authority"),
      mods = List(
        Moxie + 1,
        RepNetwork.gRep + 50),
      skills = List(
        fray.at(20),
        gunnery.at(20),
        infiltration.at(30),
        interest.anyField(30),
        kineticWeapons.at(20),
        networking.withField("Criminals").at(20),
        perception.at(20),
        pilot.withField("Spacecraft").at(40),
        profession.withField("Piracy").at(50),
        scrounging.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Survival", "+Thrill Seeking", "+Wealth", "–Authority"),
      mods = List(
        Moxie + 1,
        Aptitude.COO + 5,
        RepNetwork.gRep + 50),
      skills = List(
        art.anyField(30),
        fray.at(20),
        gunnery.at(30),
        infiltration.at(30),
        interest.anyField(40),
        intimidation.at(20),
        language.anyField(30),
        kineticWeapons.at(30),
        networking.withField("Criminals").at(30),
        perception.at(20),
        pilot.withField("Spacecraft").at(50),
        profession.withField("Piracy").at(50),
        scrounging.at(30),
        unarmedCombat.at(20))));

  val psychosurgeon = PackageGroup(
    label = "Psychosurgeon",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Helping Others", "+Neurodiversity", "–Madness"),
      mods = Nil,
      skills = List(
        academics.withField("Psychology").at(30),
        medicine.withField("Psychiatry").at(30),
        psychosurgery.at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Helping Others", "+Neurodiversity", "–Madness"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(50),
        kinesics.at(30),
        medicine.withField("Psychiatry").at(40),
        networking.anyField(35),
        perception.at(30),
        persuasion.at(20),
        profession.withField("Psychotherapy").at(40),
        psychosurgery.at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Helping Others", "+Neurodiversity", "–Madness"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Psychology").at(50),
        academics.anyField(30),
        deception.at(20),
        interest.anyField(30),
        investigation.at(25),
        kinesics.at(40),
        medicine.withField("Psychiatry").at(40),
        networking.anyField(35),
        perception.at(30),
        persuasion.at(30),
        profession.withField("Psychotherapy").at(50),
        psychosurgery.at(50))));

  val savantAsync = PackageGroup(
    label = "Savant Async",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Introspection", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi1,
        rpc(Disorders.roll(_)),
        Sleights.PsiChi + 4),
      skills = List(
        academics.anyField(30),
        investigation.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Introspection", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50)),
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi1,
        rpc(Disorders.roll(_)),
        Sleights.PsiChi + 5),
      skills = List(
        academics.anyField(40),
        art.anyField(30),
        hardware.anyField(15),
        interest.anyField(30),
        investigation.at(50),
        networking.anyField(30),
        perception.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Introspection", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        r(RepNetwork.chooseAny(_, +50)),
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi1,
        rpc(Disorders.roll(_)),
        Sleights.PsiChi + 5),
      skills = List(
        academics.anyField(40),
        art.anyField(40),
        deception.at(30),
        hardware.anyField(25),
        interest.anyField(30),
        investigation.at(50),
        kinesics.at(35),
        language.anyField(30),
        networking.anyField(25),
        perception.at(50),
        profession.anyField(30))));

  val scannerAsync = PackageGroup(
    label = "Scanner Async",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 3),
      skills = List(
        academics.anyField(30),
        sense.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        r(RepNetwork.chooseAny(_, +50)),
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 4),
      skills = List(
        academics.anyField(30),
        impersonation.at(35),
        interest.anyField(30),
        kinesics.at(35),
        perception.at(40),
        sense.at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Exploration", "+Neurodiversity", "+Personal Development", "+Self Control"),
      mods = List(
        Moxie + 1,
        Aptitude.INT + 5,
        r(RepNetwork.chooseAny(_, +50)),
        CharacterMod.BecomeAsync,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)),
        rpc(Disorders.roll(_)),
        Sleights + 6),
      skills = List(
        academics.anyField(40),
        impersonation.at(35),
        interest.anyField(40),
        investigation.at(30),
        kinesics.at(35),
        language.anyField(40),
        perception.at(50),
        persuasion.at(25),
        profession.anyField(30),
        sense.at(50))));

  val scavenger = PackageGroup(
    label = "Scavenger",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Survival", "+Wealth", "–Authority"),
      mods = Nil,
      skills = List(
        hardware.anyField(30),
        profession.withField("Scavenging").at(30),
        scrounging.at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Survival", "+Wealth", "–Authority"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.withField("Sculpture").at(40),
        hardware.anyField(40),
        networking.anyField(40),
        perception.at(35),
        pilot.anyField(40),
        profession.withField("Scavenging").at(50),
        scrounging.at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Survival", "+Wealth", "–Authority"),
      mods = List(
        Moxie + 2,
        Aptitude.COG + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(25),
        art.withField("Sculpture").at(40),
        fray.at(20),
        hardware.anyField(40),
        hardware.anyField(30),
        interest.anyField(30),
        networking.anyField(30),
        networking.anyField(30),
        perception.at(40),
        pilot.anyField(40),
        profession.withField("Scavenging").at(40),
        scrounging.at(50))));

  val scientist = PackageGroup(
    label = "Scientist",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Research", "+Science!", "+Technoprogressivism", "–Bioconservatism"),
      mods = Nil,
      skills = List(
        academics.anyField(40),
        academics.anyField(30),
        investigation.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Research", "+Science!", "+Technoprogressivism", "–Bioconservatism"),
      mods = List(RepNetwork.rRep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(30),
        investigation.at(30),
        networking.withField("Scientists").at(35),
        perception.at(40),
        profession.withField("Lab Tech").at(30),
        programming.at(30),
        research.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Research", "+Science!", "+Technoprogressivism", "–Bioconservatism"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        RepNetwork.rRep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        animalHandling.at(20),
        fray.at(15),
        hardware.withField("Electronics").at(25),
        investigation.at(50),
        language.anyField(30),
        networking.withField("Scientists").at(40),
        perception.at(40),
        profession.withField("Lab Tech").at(40),
        programming.at(40),
        research.at(40))));

  val smartAnimalHandler = PackageGroup(
    label = "Small Animal Handler",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Nano-ecology", "+Research", "+Uplift Rights"),
      mods = Nil,
      skills = List(
        animalHandling.at(40),
        medicine.withField("Veterinary").at(30),
        profession.withField("Smart Animal Training").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Nano-ecology", "+Research", "+Uplift Rights"),
      mods = List(
        Moxie + 1,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Zoology").at(40),
        animalHandling.at(50),
        fray.at(20),
        intimidation.at(20),
        kinesics.at(40),
        medicine.withField("Veterinary").at(40),
        perception.at(20),
        profession.withField("Smart Animal Training").at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Nano-ecology", "+Research", "+Uplift Rights"),
      mods = List(
        Moxie + 1,
        Aptitude.INT + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Zoology").at(40),
        academics.anyField(30),
        animalHandling.at(50),
        fray.at(30),
        intimidation.at(30),
        kinesics.at(30),
        medicine.withField("Veterinary").at(50),
        networking.anyField(30),
        perception.at(30),
        profession.withField("Smart Animal Training").at(50),
        profession.anyField(30),
        scrounging.at(30))));

  val smuggler = PackageGroup(
    label = "Smuggler",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Personal Career", "+Survival", "+Wealth", "–Authority"),
      mods = Nil,
      skills = List(
        networking.anyField(30),
        persuasion.at(40),
        profession.withField("Smuggling").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Personal Career", "+Survival", "+Wealth", "–Authority"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        deception.at(40),
        interest.withField("Black Markets").at(40),
        kinesics.at(25),
        networking.anyField(40),
        networking.anyField(20),
        perception.at(30),
        persuasion.at(50),
        profession.withField("Smuggling").at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Personal Career", "+Survival", "+Wealth", "–Authority"),
      mods = List(
        Moxie + 1,
        Aptitude.INT + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(30),
        deception.at(40),
        fray.at(30),
        interest.withField("Black Markets").at(40),
        interest.anyField(30),
        kinesics.at(25),
        networking.anyField(40),
        networking.anyField(40),
        perception.at(30),
        persuasion.at(50),
        profession.withField("Smuggling").at(50),
        protocol.at(25))));

  val soldier = PackageGroup(
    label = "Soldier",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Duty", "+Personal Development", "+Survival", "+Victory", "–Peace"),
      mods = Nil,
      skills = List(
        kineticWeapons.at(40),
        profession.withField("Squad Tactics").at(30),
        unarmedCombat.at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Duty", "+Personal Development", "+Survival", "+Victory", "–Peace"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        climbing.at(25),
        fray.at(20),
        freerunning.at(20),
        interest.anyField(40),
        kineticWeapons.at(50),
        perception.at(20),
        profession.withField("Squad Tactics").at(50),
        throwingWeapons.at(30),
        unarmedCombat.at(40))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Duty", "+Personal Development", "+Survival", "+Victory", "–Peace"),
      mods = List(
        Moxie + 1,
        Aptitude.SOM + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        blades.at(30),
        climbing.at(40),
        fray.at(30),
        freerunning.at(30),
        interest.anyField(30),
        interest.anyField(30),
        kineticWeapons.at(50),
        language.anyField(40),
        perception.at(30),
        profession.withField("Squad Tactics").at(50),
        throwingWeapons.at(30),
        unarmedCombat.at(40))));

  val spy = PackageGroup(
    label = "Spy",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Secret Identity", "–Secrets"),
      mods = Nil,
      skills = List(
        deception.at(40),
        infiltration.at(30),
        profession.withField("Spycraft").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Secret Identity", "–Secrets"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Cryptography").at(40),
        deception.at(50),
        fray.at(20),
        impersonation.at(25),
        infiltration.at(40),
        infosec.at(40),
        perception.at(30),
        profession.withField("Spycraft").at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Secret Identity", "–Secrets"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.withField("Cryptography").at(40),
        deception.at(50),
        fray.at(30),
        impersonation.at(25),
        infiltration.at(40),
        infosec.at(40),
        investigation.at(25),
        language.anyField(30),
        palming.at(30),
        perception.at(40),
        profession.withField("Spycraft").at(50))));

  val techie = PackageGroup(
    label = "Techie",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+DIY", "+Education", "+Science!", "+Sousveillance", "+Technoprogressivism"),
      mods = Nil,
      skills = List(
        hardware.anyField(40),
        interfacing.at(30),
        profession.anyField(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+DIY", "+Education", "+Science!", "+Sousveillance", "+Technoprogressivism"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        hardware.anyField(50),
        hardware.anyField(25),
        interfacing.at(40),
        infosec.at(20),
        networking.anyField(20),
        perception.at(20),
        profession.anyField(50),
        programming.at(30))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+DIY", "+Education", "+Science!", "+Sousveillance", "+Technoprogressivism"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        fray.at(15),
        hardware.anyField(50),
        hardware.anyField(40),
        interest.anyField(30),
        interfacing.at(45),
        infosec.at(25),
        language.anyField(30),
        networking.anyField(35),
        pilot.anyField(30),
        profession.anyField(50),
        programming.at(40))));

  val thief = PackageGroup(
    label = "Thief",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Wealth"),
      mods = Nil,
      skills = List(
        infiltration.at(30),
        palming.at(40),
        profession.withField("Thieving").at(30))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Wealth"),
      mods = List(RepNetwork.gRep + 50),
      skills = List(
        climbing.at(25),
        fray.at(20),
        infiltration.at(40),
        infosec.at(20),
        interest.anyField(40),
        networking.withField("Criminals").at(30),
        palming.at(50),
        perception.at(20),
        profession.withField("Thieving").at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Wealth"),
      mods = List(
        Moxie + 1,
        Aptitude.COO + 5,
        RepNetwork.gRep + 50),
      skills = List(
        art.anyField(30),
        blades.at(20),
        climbing.at(40),
        fray.at(30), hardware.withField("Electronics").at(15),
        infiltration.at(40),
        infosec.at(25),
        interest.anyField(40),
        language.anyField(30),
        networking.withField("Criminals").at(30),
        palming.at(50),
        perception.at(30),
        profession.withField("Thieving").at(50))));

  val wrecker = PackageGroup(
    label = "Wrecker",
    basic = FocusPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Explosions", "+Survival", "–TITANs"),
      mods = Nil,
      skills = List(
        demolitions.at(30),
        interest.withField("TITAN Tech").at(30),
        seekerWeapons.at(40))),
    influential = FocusPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Explosions", "+Survival", "–TITANs"),
      mods = List(r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        demolitions.at(40),
        fray.at(25),
        infiltration.at(30),
        interest.withField("TITAN Tech").at(50),
        kineticWeapons.at(30),
        perception.at(30),
        profession.withField("Squad Tactics").at(40),
        seekerWeapons.at(50))),
    formative = FocusPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Explosions", "+Survival", "–TITANs"),
      mods = List(
        Moxie + 1,
        Aptitude.COO + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        demolitions.at(40),
        fray.at(40),
        infiltration.at(30),
        infosec.at(20),
        interest.withField("TITAN Tech").at(50),
        interest.anyField(30),
        kineticWeapons.at(40),
        perception.at(30),
        profession.withField("Wrecking Machines").at(30),
        profession.withField("Squad Tactics").at(40),
        seekerWeapons.at(50),
        throwingWeapons.at(30))));
}

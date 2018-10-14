package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.{ Implicits, Random }
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium.Aptitude
import com.lkroll.ep.compendium.data.{ Disorders => DisorderData, _ }

case class BackgroundPackage(
  label:       String,
  level:       PackageLevel,
  motivations: List[Motivation]           = Nil,
  mods:        List[PackageContent]       = Nil,
  skills:      List[PackageContent.Skill] = Nil) extends Package {
  override type Self = BackgroundPackage;
  override def withPrefix(prefix: String): Self = this.copy(label = s"$prefix $label");
  override def ppCost: Int = level.ppCost;
  override def applyTo(c: Character, rand: Random): Character = {
    val motivatedChar = c.copy(motivations = c.motivations ++ motivations);
    val moddedChar = mods.foldLeft(motivatedChar) { (acc, mod) =>
      mod match {
        case PackageContent.Mod(m)      => m.applyTo(acc)
        case PackageContent.Mods(ms)    => ms.foldLeft(acc)((acc2, m) => m.applyTo(acc2))
        case PackageContent.RandMod(mf) => mf(rand).applyTo(acc)
        case PackageContent.RandMods(ms)=> ms(rand).foldLeft(acc)((acc2, m) => m.applyTo(acc2))
      }
    };
    val concreteSkills = skills.map(_.toSkill(rand));
    moddedChar.copy(skills = moddedChar.skills ++ concreteSkills)
  }

  def asFaction: FactionPackage = FactionPackage(label, level, motivations, mods, skills);
}

object BackgroundPackages {
  import Implicits.RandomArray;
  import PackageImplicits._;
  import Skills.SkillCategory;
  import Skills.Defaults.{ list => skillList, _ };
  import CharImplicits.{ skillcls2filter, skillcat2filter, string2filter, skill2filter, string2motivation };
  import RepNetwork._;

  val colonistCommandStaff = PackageGroup(
    label = "Colonist: Command Staff",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Leadership", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        persuasion.at(15),
        profession.withField("Administration").at(30),
        protocol.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Leadership", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(30),
        Skills.oneOf(freeFall, freerunning).at(35),
        interest.anyField(20),
        networking.anyField(40),
        persuasion.at(40),
        pilot.anyField(30),
        profession.withField("Administration").at(40),
        protocol.at(50))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Hard Work", "+Leadership", "+Survival"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        academics.anyField(40),
        art.anyField(40),
        beamWeapons.at(30),
        fray.at(25),
        Skills.oneOf(freeFall, freerunning).at(35),
        interest.anyField(30),
        intimidation.at(30),
        networking.anyField(40),
        persuasion.at(40),
        pilot.anyField(30),
        profession.withField("Administration").at(40),
        protocol.at(50))));

  val colonistFlightStaff = PackageGroup(
    label = "Colonist: Flight Staff",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Personal Career", "+Thrill Seeking"),
      mods = List(Moxie + 1),
      skills = List(
        pilot.withField("Spacecraft").at(40),
        profession.withField("Flight Crew").at(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Personal Career", "+Thrill Seeking"),
      mods = List(Moxie + 1),
      skills = List(
        academics.oneOf("Astrophysics", "Engineering").at(30),
        freeFall.at(40),
        hardware.withField("Aerospace").at(35),
        interest.anyField(20),
        navigation.at(40),
        networking.anyField(30),
        pilot.withField("Spacecraft").at(50),
        profession.withField("Flight Crew").at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Exploration", "+Personal Career", "+Thrill Seeking"),
      mods = List(
        Moxie + 1,
        Aptitude.REF + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        academics.oneOf("Astrophysics", "Engineering").at(30),
        fray.at(20),
        freeFall.at(40),
        gunnery.at(30),
        hardware.withField("Aerospace").at(40),
        interest.anyField(30),
        language.anyField(30),
        navigation.at(40),
        networking.anyField(30),
        networking.anyField(30),
        pilot.withField("Spacecraft").at(50),
        profession.withField("Flight Crew").at(50))));

  val colonistScienceStaff = PackageGroup(
    label = "Colonist: Science Staff",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Personal Career", "+Science!"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(40),
        Skills.oneOf(freeFall, freerunning).at(15),
        investigation.at(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Personal Career", "+Science!"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(50),
        Skills.oneOf(freeFall, freerunning).at(30),
        interest.anyField(15),
        interfacing.at(40),
        investigation.at(40),
        networking.withField("Scientists").at(40),
        profession.withField("Lab Technician").at(30),
        research.at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Hard Work", "+Personal Career", "+Science!"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        fray.at(25),
        Skills.oneOf(freeFall, freerunning).at(30),
        hardware.anyField(20),
        interest.anyField(30),
        interfacing.at(40),
        investigation.at(50),
        networking.withField("Scientists").at(40),
        profession.withField("Lab Technician").at(30),
        programming.at(35),
        research.at(40))));

  val colonistSecurityStaff = PackageGroup(
    label = "Colonist: Security Staff",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Law and Order", "+Survival", "-Criminals", "-Autonomists"),
      mods = List(Moxie + 1),
      skills = List(
        beamWeapons.at(40),
        Skills.oneOf(freeFall, freerunning).at(15),
        profession.withField("Security Ops").at(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Law and Order", "+Survival", "-Criminals", "-Autonomists"),
      mods = List(Moxie + 1),
      skills = List(
        beamWeapons.at(50),
        Skills.oneOf(freeFall, freerunning).at(40),
        clubs.at(35),
        interest.anyField(30),
        intimidation.at(40),
        language.anyField(20),
        networking.anyField(30),
        profession.withField("Security Ops").at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Law and Order", "+Survival", "–Criminals", "–Autonomists"),
      mods = List(
        Moxie + 1,
        Aptitude.SOM + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        academics.anyField(40),
        beamWeapons.at(50),
        fray.at(25),
        Skills.oneOf(freeFall, freerunning).at(40),
        clubs.at(35),
        interest.anyField(30),
        intimidation.at(40),
        investigation.at(20),
        language.anyField(30),
        networking.anyField(30),
        profession.withField("Security Ops").at(50),
        unarmedCombat.at(40))));

  val colonistTechStaff = PackageGroup(
    label = "Colonist: Tech Staff",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Problem Solving", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.oneOf(freeFall, freerunning).at(15),
        hardware.anyField(40),
        profession.anyField(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Problem Solving", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(30),
        Skills.oneOf(freeFall, freerunning).at(30),
        hardware.anyField(50),
        interest.anyField(20),
        interfacing.at(40),
        profession.anyField(40),
        programming.at(40),
        scrounging.at(35))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Hard Work", "+Problem Solving", "+Survival"),
      mods = List(
        Moxie + 1,
        Aptitude.COG + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        academics.anyField(40),
        fray.at(20),
        Skills.oneOf(freeFall, freerunning).at(30),
        hardware.anyField(50),
        hardware.anyField(40),
        interest.anyField(30),
        interfacing.at(40),
        language.anyField(30),
        pilot.anyField(25),
        profession.anyField(50),
        programming.at(40),
        scrounging.at(35))));

  val drifter = PackageGroup(
    label = "Drifter",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Hard Work", "+Pragmatism", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.oneOf(freeFall, freerunning).at(15),
        profession.anyField(30),
        scrounging.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Hard Work", "+Pragmatism", "+Survival"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.oneOf(freeFall, freerunning).at(40),
        hardware.anyField(30),
        interest.anyField(20),
        kineticWeapons.at(20),
        language.anyField(30),
        navigation.at(20),
        networking.anyField(35),
        profession.anyField(40),
        scrounging.at(50))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Exploration", "+Hard Work", "+Pragmatism", "+Survival"),
      mods = List(
        Moxie + 2,
        Aptitude.INT + 5,
        r(rand => RepNetwork.chooseAny(rand, +50))),
      skills = List(
        art.anyField(40),
        fray.at(25),
        Skills.oneOf(freeFall, freerunning).at(40),
        hardware.anyField(30),
        interest.anyField(30),
        kineticWeapons.at(20),
        language.anyField(40),
        navigation.at(20),
        networking.anyField(30),
        profession.anyField(40),
        protocol.at(20),
        scrounging.at(50))));

  val earthSurvivor = PackageGroup(
    label = "Earth Survivor",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Survival", "+/–Reclaiming Earth", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        freerunning.at(15),
        profession.withField("Post-Apocalyptic Survival").at(30),
        scrounging.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Survival", "+/–Reclaiming Earth", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        fray.at(20),
        freerunning.at(30),
        interest.anyField(20),
        infiltration.at(40),
        kineticWeapons.at(35),
        language.anyField(30),
        pilot.withField("Groundcraft").at(20),
        profession.withField("Post-Apocalyptic Survival").at(40),
        scrounging.at(50))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Survival", "+/–Reclaiming Earth", "–TITANs"),
      mods = List(
        Moxie + 2,
        Aptitude.WIL + 5,
        TraitsNegativeEP.neuralDamage),
      skills = List(
        animalHandling.at(20),
        demolitions.at(20),
        fray.at(25),
        freerunning.at(30),
        interest.anyField(30),
        infiltration.at(40),
        kineticWeapons.at(40),
        language.anyField(40),
        pilot.withField(" Groundcraft").at(25),
        profession.anyField(40),
        profession.withField("Post-Apocalyptic Survival").at(40),
        scrounging.at(50),
        seekerWeapons.at(30))));

  val fallEvacueeEnclaver = PackageGroup(
    label = "Fall Evacuee: Enclaver",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Personal Career", "+Reclaiming Earth", "+Survival", "+Wealth"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(30),
        profession.anyField(30),
        protocol.at(25))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Personal Career", "+Reclaiming Earth", "+Survival", "+Wealth"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(40),
        interest.anyField(20),
        interfacing.at(35),
        networking.anyField(50),
        persuasion.at(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        protocol.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Personal Career", "+Reclaiming Earth", "+Survival", "+Wealth"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.anyField(40),
        art.anyField(30),
        beamWeapons.at(20),
        fray.at(20),
        interest.anyField(40),
        interfacing.at(35),
        kinesics.at(25),
        networking.anyField(50),
        networking.anyField(20),
        persuasion.at(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        protocol.at(40))));

  val fallEvacueeUnderclass = PackageGroup(
    label = "Fall Evacuee: Underclass",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Personal Development", "+Survival", "–Hypercapitalism"),
      mods = List(Moxie + 1),
      skills = List(
        networking.anyField(40),
        profession.anyField(30),
        unarmedCombat.at(15))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Personal Development", "+Survival", "–Hypercapitalism"),
      mods = List(Moxie + 1),
      skills = List(
        blades.at(30),
        interest.anyField(20),
        deception.at(30),
        language.anyField(30),
        networking.anyField(50),
        persuasion.at(40),
        pilot.withField(" Groundcraft").at(25),
        profession.anyField(40),
        unarmedCombat.at(20))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Personal Development", "+Survival", "–Hypercapitalism"),
      mods = List(
        Moxie + 2,
        Aptitude.WIL + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.anyField(30),
        blades.at(30),
        fray.at(20),
        infiltration.at(30),
        interest.anyField(40),
        deception.at(30),
        language.anyField(40),
        networking.anyField(50),
        persuasion.at(40),
        pilot.withField(" Groundcraft").at(30),
        profession.anyField(40),
        unarmedCombat.at(35))));

  val hypereliteMedia = PackageGroup(
    label = "Hyperelite: Media Personality",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Artistic Expression", "+Fame", "+Personal Career"),
      mods = List(
        Moxie + 1,
        StartingCredit + 5000),
      skills = List(
        art.anyField(40),
        networking.withField("Media").at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Artistic Expression", "+Fame", "+Personal Career"),
      mods = List(
        Moxie + 1,
        StartingCredit + 30000),
      skills = List(
        art.anyField(40),
        disguise.at(25),
        interest.anyField(30),
        interfacing.at(30),
        networking.withField("Media").at(50),
        persuasion.at(30),
        profession.anyField(20),
        protocol.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Artistic Expression", "+Fame", "+Personal Career"),
      mods = List(
        Moxie + 1,
        Aptitude.SAV + 5,
        r(rand => RepNetwork.chooseAny(rand, 50)),
        StartingCredit + 60000),
      skills = List(
        art.anyField(40),
        disguise.at(25),
        fray.at(15),
        interest.anyField(40),
        interfacing.at(30),
        language.anyField(30),
        networking.withField("Media").at(50),
        networking.anyField(20),
        persuasion.at(30),
        profession.anyField(40),
        protocol.at(50))));

  val hypereliteScion = PackageGroup(
    label = "Hyperelite: Scion",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Family", "+Hypercapitalism", "+Wealth"),
      mods = List(
        Moxie + 1,
        StartingCredit + 5000),
      skills = List(
        academics.anyField(30),
        kinesics.at(30),
        protocol.at(20))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Family", "+Hypercapitalism", "+Wealth"),
      mods = List(
        Moxie + 1,
        StartingCredit + 20000),
      skills = List(
        academics.anyField(40),
        art.anyField(30),
        interest.anyField(20),
        interfacing.at(30),
        kinesics.at(50),
        networking.withField("Hypercorps").at(35),
        persuasion.at(30),
        protocol.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Family", "+Hypercapitalism", "+Wealth"),
      mods = List(
        Moxie + 1,
        StartingCredit + 50000,
        TraitsPositiveEP.patron,
        r(rand => RepNetwork.chooseAny(rand, 50)),
        Aptitude.SAV + 5),
      skills = List(
        academics.anyField(40),
        art.anyField(40),
        interest.anyField(40),
        intimidation.at(20),
        kinesics.at(50),
        kineticWeapons.at(20),
        networking.withField("Hypercorps").at(40),
        persuasion.at(30),
        profession.anyField(30),
        protocol.at(40))));

  val indenture = PackageGroup(
    label = "Indenture",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Survival", "–Hypercorps", "–Indentured Service"),
      mods = List(Moxie + 1),
      skills = List(
        hardware.anyField(40),
        language.anyField(15),
        profession.anyField(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Survival", "–Hypercorps", "–Indentured Service"),
      mods = List(Moxie + 1),
      skills = List(
        blades.at(30),
        demolitions.at(30),
        freeFall.at(40),
        hardware.anyField(50),
        interest.anyField(20),
        language.anyField(30),
        profession.anyField(40),
        scrounging.at(45))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Hard Work", "+Survival", "–Hypercorps", "–Indentured Service"),
      mods = List(
        Moxie + 2,
        Aptitude.SOM + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        blades.at(30),
        demolitions.at(30),
        fray.at(20),
        freeFall.at(40),
        hardware.anyField(50),
        interest.anyField(30),
        interfacing.at(30),
        language.anyField(40),
        networking.withField("Criminal").at(20),
        profession.anyField(40),
        profession.anyField(40),
        scrounging.at(45))));

  val infolifeEmergentUplift = PackageGroup(
    label = "Infolife: Emergent Uplift",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Mercurial Cause"),
      mods = List(
        Moxie + 1,
        TraitsNegativeTranshuman.anomalousMind,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        interest.anyField(30),
        infosec.at(25),
        interfacing.at(40),
        programming.at(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Mercurial Cause"),
      mods = List(
        Moxie + 2,
        TraitsNegativeTranshuman.anomalousMind,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.anyField(30),
        hardware.anyField(40),
        infosec.at(30),
        interest.anyField(40),
        interfacing.at(50),
        networking.anyField(30),
        profession.anyField(20),
        programming.at(40),
        research.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+AGI Rights", "+Mercurial Cause"),
      mods = List(
        Moxie + 2,
        TraitsNegativeTranshuman.anomalousMind,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)"),
        Aptitude.COG + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        Skills.chooseAny(30),
        Skills.chooseAny(30),
        academics.anyField(40),
        hardware.anyField(40),
        infosec.at(40),
        interest.anyField(40),
        interfacing.at(50),
        networking.anyField(30),
        profession.anyField(40),
        programming.at(40),
        research.at(35))));

  val infolifeHumanities = PackageGroup(
    label = "Infolife: Humanities AGI",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Personal Development", "+Philanthropy"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.oneOf("Psychology", "Sociology").at(30),
        interfacing.at(40),
        kinesics.at(35))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Personal Development", "+Philanthropy"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.oneOf("Psychology", "Sociology").at(40),
        art.withField("Digital Art").at(20),
        impersonation.at(25),
        interfacing.at(50),
        kinesics.at(40),
        networking.anyField(30),
        persuasion.at(40),
        profession.withField("Psychotherapy").at(30),
        research.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+AGI Rights", "+Personal Development", "+Philanthropy"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)"),
        Aptitude.SAV + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.oneOf("Psychology", "Sociology").at(40),
        art.withField("Digital Art").at(40),
        impersonation.at(30),
        infosec.at(30),
        interest.anyField(40),
        interfacing.at(50),
        kinesics.at(50),
        networking.anyField(40),
        persuasion.at(40),
        profession.withField("Psychotherapy").at(30),
        protocol.at(30),
        research.at(30))));

  val infolifeMachine = PackageGroup(
    label = "Infolife: Machine AGI",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Sousveillance", "+Thrill Seeking", "–Disorganization"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.anyField(30),
        interfacing.at(40),
        programming.at(35))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Sousveillance", "+Thrill Seeking", "–Disorganization"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.anyField(40),
        art.anyField(20),
        hardware.withField("Electronics").at(40),
        hardware.withField("Robotics").at(20),
        infosec.at(35),
        interfacing.at(50),
        pilot.anyField(30),
        profession.anyField(30),
        programming.at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+AGI Rights", "+Sousveillance", "+Thrill Seeking", "–Disorganization"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)"),
        Aptitude.REF + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.anyField(40),
        academics.anyField(40),
        art.anyField(30),
        flight.at(30),
        hardware.withField("Electronics").at(40),
        hardware.withField("Robotics").at(30),
        infosec.at(40),
        interfacing.at(50),
        pilot.anyField(30),
        profession.anyField(40),
        programming.at(50),
        research.at(30))));

  val infolifeResearch = PackageGroup(
    label = "Infolife: Research AGI",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Education", "+Research"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.anyField(30),
        interfacing.at(35),
        research.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Education", "+Research"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)")),
      skills = List(
        academics.anyField(40),
        academics.anyField(30),
        hardware.anyField(40),
        interfacing.at(40),
        investigation.at(30),
        networking.withField("Scientists").at(25),
        profession.anyField(20),
        programming.at(30),
        research.at(50))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+AGI Rights", "+Education", "+Research"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (AGI)"),
        Aptitude.COG + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.anyField(40),
        academics.anyField(40),
        academics.anyField(30),
        hardware.anyField(40),
        infosec.at(30),
        interfacing.at(40),
        investigation.at(40),
        networking.withField("Scientists").at(30),
        pilot.anyField(30),
        profession.anyField(40),
        programming.at(40),
        research.at(50))));

  val isolateSeparatist = PackageGroup(
    label = "Isolate: Separatist",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Artistic Expression", "+Bioconservatism", "+Religion", "+Research"),
      mods = List(Moxie + 1),
      skills = List(
        freeFall.at(15),
        profession.anyField(30),
        scrounging.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Artistic Expression", "+Bioconservatism", "+Religion", "+Research"),
      mods = List(Moxie + 1),
      skills = List(
        animalHandling.at(25),
        art.anyField(40),
        freeFall.at(30),
        hardware.anyField(40),
        medicine.withField("Paramedic").at(30),
        pilot.anyField(30),
        profession.anyField(50),
        scrounging.at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Artistic Expression", "+Bioconservatism", "+Religion", "+Research"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.realWorldNaivete,
        PickOne(Aptitude.COG, Aptitude.WIL).mapPC(_ + 5)),
      skills = List(
        academics.anyField(30),
        animalHandling.at(25),
        art.anyField(40),
        fray.at(20),
        freeFall.at(40),
        hardware.anyField(40),
        interest.anyField(30),
        interfacing.at(20),
        medicine.withField("Paramedic").at(30),
        pilot.anyField(40),
        profession.anyField(50),
        programming.at(30),
        scrounging.at(50))));

  val isolateSurvivalist = PackageGroup(
    label = "Isolate: Survivalist",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Religion", "+Survival", "–Autonomists"),
      mods = List(Moxie + 1),
      skills = List(
        freeFall.at(15),
        profession.anyField(30),
        kineticWeapons.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Religion", "+Survival", "–Autonomists"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(20),
        freeFall.at(30),
        hardware.anyField(40),
        interest.anyField(30),
        kineticWeapons.at(50),
        medicine.withField("Paramedic").at(30),
        pilot.anyField(30),
        profession.anyField(40),
        seekerWeapons.at(15))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Bioconservatism", "+Religion", "+Survival", "–Autonomists"),
      mods = List(
        Moxie + 1,
        PickOne(Aptitude.INT, Aptitude.WIL).mapPC(_ + 5)),
      skills = List(
        academics.anyField(30),
        animalHandling.at(20),
        demolitions.at(20),
        fray.at(20),
        freeFall.at(30),
        hardware.anyField(40),
        interest.anyField(340),
        kineticWeapons.at(50),
        medicine.withField("Paramedic").at(30),
        navigation.at(20),
        pilot.anyField(30),
        profession.anyField(40),
        profession.anyField(40),
        seekerWeapons.at(25))));

  val lostDisturbed = PackageGroup(
    label = "Lost: Disturbed Child",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Neurodiversity", "+Sadism", "+Vengeance", "–Research"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        TraitsNegativeEP.onTheRun,
        TraitsNegativeEP.realWorldNaivete,
        CharacterMod.BecomeAsync,
        Sleights.PsiGamma + 2),
      skills = List(
        academics.anyField(30),
        control.at(35),
        freeFall.at(15))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Neurodiversity", "+Sadism", "+Vengeance", "–Research"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)),
        TraitsNegativeEP.onTheRun,
        TraitsNegativeEP.realWorldNaivete,
        CharacterMod.BecomeAsync,
        Sleights.PsiChi + 1,
        Sleights.PsiGamma + 3),
      skills = List(
        academics.anyField(40),
        blades.at(15),
        control.at(50),
        deception.at(40),
        fray.at(15),
        freeFall.at(30),
        infiltration.at(30),
        language.anyField(30),
        profession.anyField(20))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Neurodiversity", "+Sadism", "+Vengeance", "–Research"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        TraitsNegativeEP.onTheRun,
        TraitsNegativeEP.realWorldNaivete,
        CharacterMod.BecomeAsync,
        Sleights.PsiChi + 2,
        Sleights.PsiGamma + 5,
        Aptitude.WIL + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        blades.at(20),
        control.at(50),
        deception.at(40),
        fray.at(20),
        freeFall.at(30),
        infiltration.at(30),
        interest.anyField(40),
        language.anyField(30),
        profession.anyField(40),
        psiAssault.at(40),
        unarmedCombat.at(20))));

  val lostMasked = PackageGroup(
    label = "Lost: Masked Normalcy",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Acceptance", "+Privacy", "+Self Control"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi1,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        CharacterMod.BecomeAsync,
        Sleights.PsiChi + 2), 
      skills = List(
        academics.anyField(30),
        persuasion.at(35))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Acceptance", "+Privacy", "+Self Control"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi1,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        CharacterMod.BecomeAsync,
        Sleights.PsiChi + 4,
      ), 
      skills = List(
        academics.anyField(40),
        freeFall.at(30),
        impersonation.at(40),
        kinesics.at(45),
        language.anyField(30),
        persuasion.at(50),
        profession.anyField(20))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Acceptance", "+Privacy", "+Self Control"),
      mods = List(
        Moxie + 1,
        TraitsPositiveEP.psi2,
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        rpc(Disorders.roll(_)), 
        CharacterMod.BecomeAsync,
        Sleights.PsiChi + 4,
        Sleights.PsiGamma + 2,
        TraitsNegativeEP.onTheRun,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        fray.at(20),
        freeFall.at(30),
        impersonation.at(40),
        interest.anyField(40),
        interfacing.at(30),
        kinesics.at(45),
        language.anyField(40),
        persuasion.at(50),
        profession.anyField(30),
        protocol.at(30),
        sense.at(50))));

  val originalScum = PackageGroup(
    label = "Isolate: Survivalist",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Anarchism", "+Hedonism", "+Individualism", "+Morphological Freedom"),
      mods = List(Moxie + 1),
      skills = List(
        art.anyField(30),
        freeFall.at(15),
        medicine.withField("Biosculpting").at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Anarchism", "+Hedonism", "+Individualism", "+Morphological Freedom"),
      mods = List(Moxie + 1),
      skills = List(
        art.anyField(40),
        freeFall.at(30),
        interest.anyField(30),
        language.anyField(20),
        medicine.withField("Biosculpting").at(50),
        networking.anyField(35),
        persuasion.at(40),
        psychosurgery.at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Anarchism", "+Hedonism", "+Individualism", "+Morphological Freedom"),
      mods = List(
        Moxie + 1,
        Aptitude.WIL + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        art.anyField(40),
        fray.at(20),
        freeFall.at(30),
        interest.anyField(40),
        kinesics.at(20),
        language.anyField(40),
        medicine.withField("Biosculpting").at(50),
        networking.anyField(40),
        networking.anyField(20),
        persuasion.at(40),
        profession.anyField(30),
        psychosurgery.at(40),
        sprayWeapons.at(20))));

  val reinstantiatedCivilian = PackageGroup(
    label = "Re-instantiated: Civilian Casualty",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Reclaiming Earth", "+Survival", "-TITAN Tech"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.chooseOnly(40, SkillFilter.Not(Skills.SkillCategory.Psi)),
        interfacing.at(15),
        profession.anyField(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Reclaiming Earth", "+Survival", "-TITAN Tech"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.chooseOnly(50, SkillFilter.Not(Skills.SkillCategory.Psi)),
        academics.anyField(20),
        interest.anyField(30),
        interfacing.at(45),
        networking.anyField(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        research.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Reclaiming Earth", "+Survival", "-TITAN Tech"),
      mods = List(
        Moxie + 1,
        Aptitude.INT + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        Skills.chooseOnly(50, SkillFilter.Not(Skills.SkillCategory.Psi)),
        academics.anyField(40),
        fray.at(20),
        freerunning.at(20),
        interest.anyField(40),
        interest.anyField(20),
        interfacing.at(45),
        kinesics.at(20),
        language.anyField(30),
        networking.anyField(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        research.at(30))));

  val reinstantiatedInformorph = PackageGroup(
    label = "Re-instantiated: Infomorph",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Reclaiming Earth", "+Survival", "+/–Virtual Reality", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        interfacing.at(40),
        profession.anyField(30),
        programming.at(15))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Reclaiming Earth", "+Survival", "+/–Virtual Reality", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        academics.anyField(20),
        interest.anyField(30),
        interfacing.at(50),
        networking.anyField(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        programming.at(45),
        research.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Reclaiming Earth", "+Survival", "+/–Virtual Reality", "–TITANs"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.editedMemories,
        Aptitude.COG + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        Skills.chooseOnly(40, SkillFilter.Not(SkillCategory.Psi)),
        academics.anyField(30),
        fray.at(10),
        infosec.at(20),
        interest.anyField(50),
        interest.anyField(40),
        interfacing.at(50),
        networking.anyField(40),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        programming.at(45),
        research.at(30))));

  val reinstantiatedMilitary = PackageGroup(
    label = "Re-instantiated: Military Casualty",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Reclaiming Earth", "+Survival", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.chooseOnly(40, SkillCategory.Combat),
        freerunning.at(15),
        profession.anyField(30))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Reclaiming Earth", "+Survival", "–TITANs"),
      mods = List(Moxie + 1),
      skills = List(
        Skills.chooseOnly(50, SkillCategory.Combat),
        academics.anyField(20),
        fray.at(20),
        freerunning.at(35),
        gunnery.at(30),
        interest.anyField(30),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        unarmedCombat.at(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Reclaiming Earth", "+Survival", "–TITANs"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.editedMemories,
        Aptitude.INT + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        Skills.chooseOnly(50, SkillCategory.Combat),
        Skills.chooseOnly(50, SkillCategory.Combat),
        academics.anyField(30),
        climbing.at(30),
        fray.at(25),
        freerunning.at(35),
        gunnery.at(30),
        interest.anyField(50),
        language.anyField(30),
        pilot.withField("Groundcraft").at(15),
        profession.anyField(40),
        unarmedCombat.at(40))));

  val streetRat = PackageGroup(
    label = "Street Rat",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Black Markets", "+Cartel/Gang/Family", "+Survival", "+Wealth", "–Law and Order", "–Police"),
      mods = List(Moxie + 1),
      skills = List(
        networking.withField("Criminals").at(15),
        profession.anyField(30),
        unarmedCombat.at(40))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Black Markets", "+Cartel/Gang/Family", "+Survival", "+Wealth", "–Law and Order", "–Police"),
      mods = List(Moxie + 1),
      skills = List(
        clubs.at(30),
        fray.at(20),
        infiltration.at(15),
        interest.anyField(20),
        intimidation.at(30),
        language.anyField(40),
        networking.withField("Criminals").at(30),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(30),
        unarmedCombat.at(40))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Black Markets", "+Cartel/Gang/Family", "+Survival", "+Wealth", "–Law and Order", "–Police"),
      mods = List(
        Moxie + 1,
        Aptitude.SOM + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        clubs.at(30),
        deception.at(25),
        fray.at(30),
        infiltration.at(25),
        interest.anyField(40),
        interest.anyField(30),
        intimidation.at(40),
        language.anyField(40),
        networking.withField("Criminals").at(40),
        palming.at(20),
        pilot.withField("Groundcraft").at(30),
        profession.anyField(40),
        unarmedCombat.at(40))));

  val upliftEscapee = PackageGroup(
    label = "Uplift: Escapee",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Privacy", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)")),
      skills = List(
        Skills.oneOf(climbing, swimming, flight, freerunning).at(40), // TODO technically based on uplift type
        interest.anyField(30),
        infiltration.at(25))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Privacy", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        TraitsNegativeEP.onTheRun),
      skills = List(
        Skills.oneOf(climbing, swimming, flight, freerunning).at(45), // TODO technically based on uplift type
        deception.at(40),
        infiltration.at(50),
        impersonation.at(40),
        interest.anyField(40),
        language.anyField(20),
        networking.anyField(40),
        profession.anyField(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Privacy", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        TraitsNegativeEP.onTheRun,
        Aptitude.SAV + 5,
        r(rand => RepNetwork.chooseAny(rand, 50))),
      skills = List(
        academics.anyField(40),
        Skills.oneOf(climbing, swimming, flight, freerunning).at(50), // TODO technically based on uplift type
        deception.at(40),
        fray.at(25),
        infiltration.at(50),
        impersonation.at(40),
        interest.anyField(40),
        language.anyField(40),
        networking.anyField(40),
        palming.at(25),
        profession.anyField(30),
        unarmedCombat.at(30))));

  val upliftFeral = PackageGroup(
    label = "Uplift: Feral",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Mercurial Cause", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        TraitsNegativeTranshuman.anomalousMind,
        TraitsPositiveTranshuman.heighenedInstinct),
      skills = List(
        Skills.oneOf(climbing, swimming, flight, freerunning).at(40), // TODO technically based on uplift type
        interest.anyField(30),
        unarmedCombat.at(35))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Mercurial Cause", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 2,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        TraitsNegativeTranshuman.anomalousMind,
        TraitsPositiveTranshuman.heighenedInstinct),
      skills = List(
        Skills.oneOf(climbing, swimming, flight, freerunning).at(50), // TODO technically based on uplift type
        fray.at(40),
        infiltration.at(20),
        interest.anyField(45),
        intimidation.anyField(40),
        unarmedCombat.at(50))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Mercurial Cause", "+Survival", "+Uplift Rights", "–Hypercorps", "–Uplift Slavery"),
      mods = List(
        Moxie + 2,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        TraitsNegativeTranshuman.anomalousMind,
        TraitsPositiveTranshuman.heighenedInstinct,
        Aptitude.REF + 5),
      skills = List(
        art.anyField(30),
        Skills.oneOf(climbing, swimming, flight, freerunning).at(50), // TODO technically based on uplift type
        fray.at(40),
        infiltration.at(50),
        interest.anyField(40),
        intimidation.at(50),
        networking.anyField(30),
        scrounging.at(20),
        unarmedCombat.at(50))));

  val upliftStandard = PackageGroup(
    label = "Uplift: Standard Specimen",
    basic = BackgroundPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Mercurial Cause", "+Sapient Cause", "+Uplift Rights", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)")),
      skills = List(
        academics.anyField(30),
        Skills.oneOf(climbing, swimming, flight, freerunning).at(45), // TODO technically based on uplift type
        interfacing.at(20))),
    influential = BackgroundPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Mercurial Cause", "+Sapient Cause", "+Uplift Rights", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)")),
      skills = List(
        academics.anyField(40),
        Skills.oneOf(climbing, swimming, flight, freerunning).at(50), // TODO technically based on uplift type
        fray.at(20),
        interest.anyField(20),
        interfacing.at(30),
        intimidation.at(30),
        kinesics.at(35),
        networking.anyField(40),
        profession.anyField(30))),
    formative = BackgroundPackage(
      label = "5PP",
      level = PackageLevel.Formative,
      motivations = List("+Mercurial Cause", "+Sapient Cause", "+Uplift Rights", "–Uplift Slavery"),
      mods = List(
        Moxie + 1,
        TraitsNegativeEP.socialStigma.copy(name = "Social Stigma (Uplift)"),
        Aptitude.COG + 5,
        r(RepNetwork.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        Skills.oneOf(climbing, swimming, flight, freerunning).at(50), // TODO technically based on uplift type
        fray.at(20),
        interest.anyField(40),
        interfacing.at(30),
        intimidation.at(30),
        kinesics.at(40),
        language.anyField(30),
        networking.anyField(40),
        persuasion.at(30),
        profession.anyField(40),
        protocol.at(30),
        unarmedCombat.at(20))));
}

package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium.{ Aptitude, Motivation }
import com.lkroll.ep.compendium.data.DefaultSkills

case class FactionPackage(
  label:       String,
  level:       PackageLevel,
  motivations: List[Motivation]           = Nil,
  mods:        List[PackageContent]       = Nil,
  skills:      List[PackageContent.Skill] = Nil) extends GroupedPackage {
  override type Self = FactionPackage;
  override def withPrefix(prefix: String): Self = this.copy(label = s"$prefix $label");
  override def ppCost: Int = level.ppCost;
  override def applyTo(c: CharGenCharacter, rand: Random): CharGenCharacter = {
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
    val generalLabel = label.split("""\d""")(0);
    moddedChar.copy(skills = moddedChar.skills ++ concreteSkills, faction = s"$generalLabel & ${c.faction}")
  }
}

object FactionPackages {
  import Implicits.RandomArray;
  import PackageImplicits._;
  import DefaultSkills.{ list => skillList, _ };
  import CharImplicits.{ RepNetworkExt, skillcls2filter, skillcat2filter, string2filter, skill2filter, string2motivation, skilldef2skill };
  import RepNetworks._;

  val anarchist = PackageGroup(
    label = "Anarchist",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Anarchism", "+Community", "+Liberty", "+Morphological Freedom", "–Authority", "–Hypercapitalism"),
      mods = Nil,
      skills = List(
        academics.withField("Political Science").at(30),
        kineticWeapons.at(30),
        networking.withField("Autonomists").at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Anarchism", "+Community", "+Liberty", "+Morphological Freedom", "–Authority", "–Hypercapitalism"),
      mods = List(
        Moxie + 1,
        circleARep + 50),
      skills = List(
        academics.withField("Political Science").at(50),
        freeFall.at(30),
        infosec.at(30),
        interest.anyField(40),
        kineticWeapons.at(20), networking.withField("Autonomists").at(50),
        persuasion.at(30),
        scrounging.at(30))));

  val argonaut = PackageGroup(
    label = "Argonaut",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Open Source", "+Research", "+Technoprogressivism"),
      mods = Nil,
      skills = List(
        academics.anyField(40),
        investigation.at(30),
        research.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Open Source", "+Research", "+Technoprogressivism"),
      mods = List(
        Moxie + 1,
        rRep + 50),
      skills = List(
        academics.anyField(50),
        interest.anyField(30),
        investigation.at(40),
        profession.anyField(40),
        networking.withField("Scientists").at(40),
        programming.at(40),
        research.at(40))));

  val barsoomian = PackageGroup(
    label = "Barsoomian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Anarchism", "+Barsoomian Movement", "+Community", "+Technoprogressivism", "–Hypercorps"),
      mods = Nil,
      skills = List(
        navigation.at(30),
        pilot.anyField(40),
        profession.anyField(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Anarchism", "+Barsoomian Movement", "+Community", "+Technoprogressivism", "–Hypercorps"),
      mods = List(circleARep + 50),
      skills = List(
        hardware.anyField(35),
        interest.anyField(40),
        kineticWeapons.at(30),
        navigation.at(30),
        networking.withField("Autonomists").at(40),
        pilot.anyField(40),
        profession.anyField(50),
        scrounging.at(30))));

  val belter = PackageGroup(
    label = "Belter",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Extropianism", "+Hypercapitalism", "+Morphological Freedom"),
      mods = Nil,
      skills = List(
        freeFall.at(40),
        navigation.at(30),
        profession.withField("Asteroid Mining").at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Extropianism", "+Hypercapitalism", "+Morphological Freedom"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        freeFall.at(50),
        interest.anyField(40),
        kineticWeapons.at(30),
        navigation.at(30),
        networking.anyField(30),
        profession.withField("Asteroid Mining").at(50),
        persuasion.at(20),
        pilot.withField("Spacecraft").at(30))));

  val bioconservative = PackageGroup(
    label = "Bioconservative",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Religion", "–AGI Rights", "–Uplift Rights", "–X-Risks"),
      mods = Nil,
      skills = List(
        freerunning.at(30),
        interest.anyField(40),
        pilot.anyField(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Religion", "–AGI Rights", "–Uplift Rights", "–X-Risks"),
      mods = Nil,
      skills = List(
        demolitions.at(30),
        freerunning.at(40),
        interest.anyField(50),
        kineticWeapons.at(40),
        medicine.withField("Paramedic").at(30),
        pilot.anyField(40),
        profession.anyField(40),
        scrounging.at(30))));

  val brinker = PackageGroup(
    label = "Brinker",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Exhumanism", "+Privacy", "+Religion", "+Solitude", "+Self Reliance"),
      mods = Nil,
      skills = List(
        pilot.withField("Spacecraft").at(40),
        profession.anyField(30),
        scrounging.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Exhumanism", "+Privacy", "+Religion", "+Solitude", "+Self Reliance"),
      mods = List(r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        freeFall.at(30),
        gunnery.at(20),
        hardware.withField("Industrial").at(40),
        interest.anyField(40),
        networking.anyField(25),
        pilot.withField("Spacecraft").at(50),
        profession.anyField(50),
        scrounging.at(40))));

  val criminal = PackageGroup(
    label = "Criminal",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Survival", "+Thrill Seeking", "+Wealth", "–Law and Order"),
      mods = Nil,
      skills = List(
        interest.withField("Criminal Groups").at(30),
        palming.at(30),
        intimidation.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Survival", "+Thrill Seeking", "+Wealth", "–Law and Order"),
      mods = List(gRep + 50),
      skills = List(
        deception.at(30),
        infiltration.at(30),
        interest.withField("Criminal Groups").at(50),
        intimidation.at(40),
        networking.withField("Criminals").at(40),
        palming.at(30),
        profession.anyField(40), unarmedCombat.at(35))));

  val earthSurvivor = BackgroundPackages.earthSurvivor.asFaction;

  val europan = PackageGroup(
    label = "Europan",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration, +Morphological Freedom, +Uplift Rights, –Bioconservatism"),
      mods = Nil,
      skills = List(
        academics.anyField(30),
        pilot.withField("Submarine").at(30),
        swimming.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration, +Morphological Freedom, +Uplift Rights, –Bioconservatism"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        academics.anyField(40),
        academics.anyField(20),
        interfacing.at(30),
        navigation.at(35),
        networking.anyField(30),
        pilot.withField("Submarine").at(40),
        profession.anyField(35),
        swimming.at(50))));

  val exhuman = PackageGroup(
    label = "Exhuman",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exhumanism", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = Nil,
      skills = List(
        interest.withField("Exhumans").at(30),
        medicine.withField("Biosculpting").at(40),
        psychosurgery.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exhumanism", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = List(Moxie + 1),
      skills = List(
        academics.withField("Genetics").at(40),
        disguise.at(15),
        interest.withField("Exhumans").at(50),
        intimidation.at(30),
        medicine.withField("Biosculpting").at(50),
        medicine.anyField(30),
        psychosurgery.at(30),
        unarmedCombat.at(40))));

  val extropian = PackageGroup(
    label = "Extropian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Extropianism", "+Morphological Freedom", "+Personal Development", "–Bioconservativism"),
      mods = Nil,
      skills = List(
        interest.withField("Cutting-Edge Technology").at(30),
        networking.withField("Autonomists").at(30),
        persuasion.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Extropianism", "+Morphological Freedom", "+Personal Development", "–Bioconservativism"),
      mods = List(
        circleARep + 50,
        cRep + 50),
      skills = List(
        freeFall.at(20),
        interest.withField("Cutting-Edge Technology").at(40),
        interfacing.at(20),
        kinesics.at(30),
        networking.withField("Autonomists").at(40),
        networking.withField("Hypercorps").at(40),
        persuasion.at(50),
        profession.anyField(50))));

  val hypercorp = PackageGroup(
    label = "Hypercorp",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hypercapitalism", "+Stability", "+Wealth", "–Anarchism", "–AGI Rights", "–Uplift Rights"),
      mods = Nil,
      skills = List(
        academics.withField("Economics").at(30),
        persuasion.at(30),
        protocol.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hypercapitalism", "+Stability", "+Wealth", "–Anarchism", "–AGI Rights", "–Uplift Rights"),
      mods = List(
        Moxie + 1,
        cRep + 50),
      skills = List(
        academics.withField("Economics").at(50),
        interfacing.at(40),
        networking.withField("Hypercorps").at(40),
        networking.withField("Media").at(20),
        persuasion.at(40),
        profession.anyField(40),
        protocol.at(50))));

  val jovian = PackageGroup(
    label = "Jovian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Jovian Republic", "–AGI Rights", "–Transhumanism", "–Uplift Rights"),
      mods = Nil,
      skills = List(
        pilot.anyField(30),
        profession.withField("Military Ops").at(30),
        seekerWeapons.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Jovian Republic", "–AGI Rights", "–Transhumanism", "–Uplift Rights"),
      mods = List(
        Moxie + 1,
        cRep + 50),
      skills = List(
        academics.withField("Military Science").at(30),
        intimidation.at(25),
        kineticWeapons.at(40),
        language.oneOf("English", "Spanish").at(40),
        networking.withField("Hypercorps").at(40),
        profession.withField("Military Ops").at(30),
        seekerWeapons.at(40),
        unarmedCombat.at(35))));

  val lunar = PackageGroup(
    label = "Lunar",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Hypercapitalism", "+Preserving Traditions", "+Reclaiming Earth"),
      mods = Nil,
      skills = List(
        art.anyField(40),
        language.anyField(30),
        protocol.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Hypercapitalism", "+Preserving Traditions", "+Reclaiming Earth"),
      mods = List(
        Moxie + 1,
        cRep + 50),
      skills = List(
        academics.withField("Pre-Fall History").at(50),
        art.anyField(40),
        interfacing.at(30),
        kinesics.at(40),
        language.anyField(40),
        networking.withField("Hypercorps").at(40),
        protocol.at(40))));

  val mercurialInfolife = PackageGroup(
    label = "Mercurial: Infolife",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Mercurial Cause", "–Assimilation", "–Bioconservatism", "–Sapient Cause"),
      mods = Nil,
      skills = List(
        Skills.chooseAny(40),
        interest.withField("Infolife Clades").at(30),
        interfacing.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Mercurial Cause", "–Assimilation", "–Bioconservatism", "–Sapient Cause"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        Skills.chooseAny(50),
        interest.withField("Infolife Clades").at(50),
        interest.anyField(40),
        interfacing.at(30),
        intimidation.at(20),
        networking.anyField(30),
        persuasion.at(30),
        programming.at(30))));

  val mercurialUplift = PackageGroup(
    label = "Mercurial: Uplift",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Mercurial Cause", "+Uplift Rights", "–Assimilation", "–Bioconservatism", "–Sapient Cause"),
      mods = Nil,
      skills = List(
        Skills.chooseAny(40),
        interest.anyField(30),
        interest.withField("Uplift Clades").at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Mercurial Cause", "+Uplift Rights", "–Assimilation", "–Bioconservatism", "–Sapient Cause"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        Skills.chooseAny(50),
        interest.withField("Uplift Clades").at(50),
        interest.anyField(40),
        intimidation.at(20),
        medicine.withField("Uplifts").at(30),
        networking.anyField(30),
        persuasion.at(30),
        unarmedCombat.at(30))));

  val nanoEcologist = PackageGroup(
    label = "Nano-Ecologist",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Nano-ecology", "+Research", "+Technoprogressivism"),
      mods = Nil,
      skills = List(
        academics.oneOf("Ecology", "Nanotechnology").at(30),
        freerunning.at(30),
        programming.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Nano-ecology", "+Research", "+Technoprogressivism"),
      mods = List(eRep + 50),
      skills = List(
        academics.withField("Ecology").at(40),
        academics.withField("Nanotechnology").at(50),
        freerunning.at(30),
        interfacing.at(25),
        investigation.at(30),
        medicine.withField("Nanomedicine").at(30),
        networking.withField ("Ecologists").at(40),
        programming.at(50))));

  val orbital = PackageGroup(
    label = "Orbital",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Precautionism", "+Reclaiming Earth", "–AGI Rights"),
      mods = Nil,
      skills = List(
        freeFall.at(30),
        language.anyField(30),
        pilot.anyField(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Precautionism", "+Reclaiming Earth", "–AGI Rights"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        clubs.at(20),
        freeFall.at(35),
        interest.anyField(40),
        interfacing.at(25),
        intimidation.at(20),
        language.anyField(50),
        networking.anyField(40),
        pilot.anyField(50))));

  val outster = PackageGroup(
    label = "Out'ster",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Exploration", "+Morphological Freedom", "+Research"),
      mods = Nil,
      skills = List(
        freeFall.at(30),
        interest.withField("Simulspace").at(30),
        interfacing.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Exploration", "+Morphological Freedom", "+Research"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        freeFall.at(30),
        infosec.at(20),
        interest.withField("Simulspace").at(50),
        interest.anyField(40),
        interfacing.at(50),
        pilot.withField("Spacecraft").at(25),
        programming.at(40),
        psychosurgery.at(25))));

  val precautionist = PackageGroup(
    label = "Precautionist",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Precautionism", "+Reclaiming Earth", "–Bioconservatism", "–Technoprogressivism"),
      mods = Nil,
      skills = List(
        academics.anyField(30),
        infosec.at(30),
        research.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Precautionism", "+Reclaiming Earth", "–Bioconservatism", "–Technoprogressivism"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        hardware.anyField(20),
        infosec.at(20),
        interfacing.at(30),
        investigation.at(40),
        networking.anyField(30),
        research.at(50))));

  val preservationist = PackageGroup(
    label = "Preservationist",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Environmentalism", "+Preservationism", "+Research", "–Gatecrashing", "–Nano-ecology"),
      mods = Nil,
      skills = List(
        academics.withField("Ecology").at(40),
        freerunning.at(30),
        investigation.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Environmentalism", "+Preservationism", "+Research", "–Gatecrashing", "–Nano-ecology"),
      mods = List(eRep + 50),
      skills = List(
        academics.withField("Ecology").at(50),
        art.anyField(30),
        freerunning.at(40),
        interest.anyField(30),
        investigation.at(40),
        medicine.anyField(30),
        navigation.at(35),
        networking.withField("Ecologists").at(40))));

  val reclaimer = PackageGroup(
    label = "Reclaimer",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Bioconservatism", "+Reclaiming Earth", "–AGI Rights"),
      mods = Nil,
      skills = List(
        freerunning.at(30),
        hardware.anyField(40),
        language.anyField(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Bioconservatism", "+Reclaiming Earth", "–AGI Rights"),
      mods = List(r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        demolitions.at(20),
        freerunning.at(30),
        hardware.anyField(50),
        infosec.at(30),
        language.anyField(50),
        language.anyField(40),
        networking.anyField(20),
        pilot.anyField(25),
        seekerWeapons.at(30))));

  val ringer = PackageGroup(
    label = "Ringer",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = Nil,
      skills = List(
        flight.at(40),
        interest.anyField(30),
        scrounging.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = List(r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        academics.anyField(50),
        beamWeapons.at(25),
        flight.at(50),
        freeFall.at(40),
        interest.anyField(40),
        networking.anyField(30),
        pilot.withField("Spacecraft").at(30),
        scrounging.at(30))));

  val sapient = PackageGroup(
    label = "Sapient",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+AGI Rights", "+Sapient Cause", "+Uplift Rights", "–Bioconservatism"),
      mods = Nil,
      skills = List(
        interest.anyField(30),
        kinesics.at(30),
        protocol.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+AGI Rights", "+Sapient Cause", "+Uplift Rights", "–Bioconservatism"),
      mods = List(r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        interest.anyField(50),
        interest.anyField(40),
        kinesics.at(40),
        medicine.withField("Uplifts").at(20),
        networking.anyField(30),
        persuasion.at(40),
        protocol.at(50),
        psychosurgery.at(25))));

  val scum = PackageGroup(
    label = "Scum",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Anarchism", "+Hedonism", "+Individualism", "+Morphological Freedom"),
      mods = Nil,
      skills = List(
        art.anyField(30),
        freeFall.at(30),
        networking.withField("Autonomists").at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Anarchism", "+Hedonism", "+Individualism", "+Morphological Freedom"),
      mods = List(
        Moxie + 1,
        circleARep + 50),
      skills = List(
        art.anyField(50),
        freeFall.at(40),
        interest.anyField(40),
        kinesics.at(40),
        medicine.withField("Biosculpting").at(30),
        networking.withField("Autonomists").at(50),
        persuasion.at(30))));

  val sifter = PackageGroup(
    label = "Sifter",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Mercurian Independence", "–Hypercapitalism"),
      mods = Nil,
      skills = List(
        navigation.at(30),
        pilot.withField("Groundcraft").at(40),
        profession.withField("Mining").at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Mercurian Independence", "–Hypercapitalism"),
      mods = List(
        Moxie + 1,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        climbing.at(25),
        interest.anyField(40),
        navigation.at(40),
        networking.anyField(30),
        palming.at(25),
        pilot.withField("Groundcraft").at(50),
        profession.withField("Mining").at(50),
        unarmedCombat.at(20))));

  val singularitySeeker = PackageGroup(
    label = "Singularity Seeker",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = Nil,
      skills = List(
        infosec.at(30),
        interest.withField("TITAN Tech").at(40),
        interfacing.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = Nil,
      skills = List(
        academics.anyField(40),
        hardware.anyField(40),
        infosec.at(40),
        interest.withField("TITAN Tech").at(50),
        interfacing.at(30),
        medicine.withField("Nanomedicine").at(30),
        programming.at(40),
        psychosurgery.at(30))));

  val skimmer = PackageGroup(
    label = "Skimmer",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hard Work", "+Independence", "+Thrill Seeking"),
      mods = Nil,
      skills = List(
        flight.at(30),
        interest.anyField(30),
        pilot.withField("Aircraft").at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hard Work", "+Independence", "+Thrill Seeking"),
      mods = List(r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        flight.at(40),
        gunnery.at(30),
        interest.anyField(40),
        navigation.at(35),
        networking.anyField(30),
        pilot.withField("Aircraft").at(40),
        pilot.withField("Spacecraft").at(30),
        profession.withField("Gas Mining").at(50))));

  val socialite = PackageGroup(
    label = "Socialite",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Artistic Expression", "+Fame", "+Hypercapitalism", "+Wealth", "–Anarchism"),
      mods = Nil,
      skills = List(
        art.anyField(30),
        networking.withField("Media").at(40),
        kinesics.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Artistic Expression", "+Fame", "+Hypercapitalism", "+Wealth", "–Anarchism"),
      mods = List(fRep + 50),
      skills = List(
        art.anyField(40),
        deception.at(25),
        interest.anyField(50),
        intimidation.at(30),
        kinesics.at(40),
        networking.withField("Media").at(40),
        persuasion.at(30), protocol.at(40))));

  val solarian = PackageGroup(
    label = "Solarian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = Nil,
      skills = List(
        flight.at(40),
        interest.anyField(30),
        medicine.withField("Nanomedicine").at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Exploration", "+Morphological Freedom", "+Personal Development", "+Research"),
      mods = List(
        Moxie + 2,
        r(RepNetworks.chooseAny(_, +50))),
      skills = List(
        flight.at(50),
        interest.anyField(50),
        interest.anyField(40),
        medicine.withField("Nanomedicine").at(40),
        navigation.at(30),
        networking.anyField(30),
        pilot.withField("Spacecraft").at(25))));

  val titanian = PackageGroup(
    label = "Titanian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Research", "+Technosocialism", "–Bioconservatism", "–Hypercapitalism"),
      mods = Nil,
      skills = List(
        academics.anyField(30),
        networking.withField("Autonomists").at(40),
        programming.at(30))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Research", "+Technosocialism", "–Bioconservatism", "–Hypercapitalism"),
      mods = List(circleARep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        interfacing.at(25),
        networking.withField("Autonomists").at(50),
        persuasion.at(30),
        pilot.withField("Aircraft").at(30),
        programming.at(40),
        research.at(30))));

  val ultimate = PackageGroup(
    label = "Ultimate",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hypercapitalism", "+Personal Development", "+Ultimates", "–Bioconservatism"),
      mods = Nil,
      skills = List(
        academics.anyField(30),
        freerunning.at(30),
        unarmedCombat.at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hypercapitalism", "+Personal Development", "+Ultimates", "–Bioconservatism"),
      mods = List(
        cRep + 50,
        uRep + 50),
      skills = List(
        academics.anyField(50),
        academics.anyField(40),
        blades.at(25),
        climbing.at(30),
        freerunning.at(40),
        intimidation.at(30),
        kineticWeapons.at(30),
        unarmedCombat.at(50))));

  val venusian = PackageGroup(
    label = "Venusian",
    basic = FactionPackage(
      label = "1PP",
      level = PackageLevel.Basic,
      motivations = List("+Hypercapitalism", "+Personal Development", "+Uplift Rights"),
      mods = Nil,
      skills = List(
        academics.anyField(30),
        networking.withField("Hypercorps").at(30),
        pilot.withField("Aircraft").at(40))),
    influential = FactionPackage(
      label = "3PP",
      level = PackageLevel.Influential,
      motivations = List("+Hypercapitalism", "+Personal Development", "+Uplift Rights"),
      mods = List(
        Moxie + 1,
        cRep + 50),
      skills = List(
        academics.anyField(50),
        beamWeapons.at(25),
        kinesics.at(30),
        navigation.at(25),
        networking.withField("Hypercorps").at(40),
        pilot.withField("Aircraft").at(40),
        profession.anyField(40),
        protocol.at(30))));
}

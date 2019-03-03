package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.{ Random, UniqueGear }
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._
import com.lkroll.common.macros.Macros

import scala.language.implicitConversions

case class GearPackage(
  label:         String,
  cpCost:        Int,
  requirements:  List[PackageRequirement]             = Nil,
  gear:          List[GearEntry]                      = Nil,
  armour:        List[Either[Armour, ModdedArmour]]   = Nil,
  weapons:       List[Either[Weapon, WeaponWithAmmo]] = Nil,
  augmentations: List[Augmentation]                   = Nil,
  software:      List[Software]                       = Nil) extends GeneralPackage {

  def creditCost: Int = cpCost * 1000;

  def usableBy(char: CharGenCharacter): Boolean = {
    requirements.forall(r => r.fulfilledBy(char))
  }

  override def applyTo(c: CharGenCharacter, rand: Random): CharGenCharacter = {
    val moddedMorph = c.activeMorph.copy(
      enhancements = (c.activeMorph.enhancements ++ augmentations.map(_.name)));
    c.copy(
      activeMorph = moddedMorph,
      gear = c.gear ++ gear,
      armour = c.armour ++ armour,
      weapons = c.weapons ++ weapons,
      software = c.software ++ software)
  }
}

sealed trait PackageRequirement {
  def fulfilledBy(char: CharGenCharacter): Boolean;
}
object PackageRequirement {
  case class MorphTypeIn(types: Set[MorphType]) extends PackageRequirement {
    override def fulfilledBy(char: CharGenCharacter): Boolean = types.contains(char.activeMorph.morphType);
  }
  case class SkillAtLeast(skill: Skills.Skill, minRanks: Int) extends PackageRequirement {
    override def fulfilledBy(char: CharGenCharacter): Boolean = {
      char.skills.find(s => s.skillDef.matches(skill)).map(s => s.ranks >= minRanks).getOrElse(false)
    }
  }
  case class SkillChoiceAtLeast(skill: PackageContent.Skill, minRanks: Int) extends PackageRequirement {
    override def fulfilledBy(char: CharGenCharacter): Boolean = {
      char.skills.find(s => skill.matches(s.skillDef)).map(s => s.ranks >= minRanks).getOrElse(false)
    }
  }
}

object GearImplicits {
  implicit class GearExt(gear: Gear) {
    def *(i: Int): GearEntry = GearEntry(gear, i);
  }

  implicit def gear2entry(gear: Gear): GearEntry = gear * 1;

  implicit class SkillDefExt(skillDef: SkillDef) {
    def >=(ranks: Int): PackageRequirement.SkillAtLeast = PackageRequirement.SkillAtLeast(CharImplicits.skilldef2skill(skillDef), ranks);
  }

  implicit class SkillExt(skillDef: Skills.Skill) {
    def >=(ranks: Int): PackageRequirement.SkillAtLeast = PackageRequirement.SkillAtLeast(skillDef, ranks);
  }

  implicit class PackageSkillExt(skill: PackageContent.Skill) {
    def >=(ranks: Int): PackageRequirement.SkillChoiceAtLeast = PackageRequirement.SkillChoiceAtLeast(skill, ranks);
  }

  def morphType(types: MorphType*): PackageRequirement.MorphTypeIn = PackageRequirement.MorphTypeIn(types.toSet);

  implicit def autoEitherL[L, R](l: L): Either[L, R] = Left(l);
  implicit def autoEitherR[L, R](r: R): Either[L, R] = Right(r);

  implicit class SubstanceExt(subst: Substance) {
    def *(i: Int): GearEntry = GearEntry(Gear(
      name = subst.name,
      category = subst.category,
      descr = subst.descr,
      price = subst.price,
      source = subst.source,
      sourcePage = subst.sourcePage), i);
  }
}

// TODO do properly in compendium
object Bots {
  val automech = UniqueGear("Automech Bot");
  val caretaker = UniqueGear("Caretaker Bot"); // Panopticon p.157
  val creepy = UniqueGear("Creepy Bot");
  val explorenaut = UniqueGear("Explorenaut"); // Gatecrashing p.160
  val gnat = UniqueGear("Gnat Bot");
  val guardianAngel = UniqueGear("Guardian Angel Bot");
  val reconFlyer = UniqueGear("Recon Flyer");
  val robomule = UniqueGear("Robomule"); // Gatecrashing p.162
  val servitor = UniqueGear("Servitor Bot");
  val speck = UniqueGear("Speck Bot");
}
object Services {
  val anonymousAccounts = UniqueGear("Anonymous Accounts");
  val backup = UniqueGear("Backup");
  val fakeEgoID = UniqueGear("Fake Ego ID");
}
object Pets {
  val guardDog = UniqueGear("Guard Dog"); // Panopticon p.154
  val smartHawk = UniqueGear("Smart Hawk");
  val smartRacoon = UniqueGear("Smart Racoon");
  val smartRat = UniqueGear("Smart Rat");
  val spaceRoach = UniqueGear("Space Roach");
}
object OtherTodo {
  val mobileBase = UniqueGear("Mobile Base"); // Gatecrashing p.164
}

object GearPackages {
  import GearImplicits._;
  import DefaultSkills._;
  import CharImplicits.skilldef2skill;

  val botJammer = GearPackage(
    label = "Bot Jammer",
    cpCost = 4,
    requirements = List(
      hardware.withField("Robotics") >= 30,
      pilot.oneOf("Groundcraft", "Aircraft", "Anthroform") >= 30),
    gear = List(
      Bots.automech,
      Bots.gnat * 2,
      Bots.guardianAngel,
      CommunicationsGear.radioBooster,
      EverydayTech.toolKit.copy(name = "Robotics Tool Kit"),
      Bots.servitor,
      Bots.speck * 3));

  val combatMorph = GearPackage(
    label = "Combat Morph",
    cpCost = 11,
    requirements = List(morphType(MorphType.Biomorph, MorphType.Pod)),
    augmentations = List(
      Cyberware.antiGlare,
      Bioware.lightBioweaveArmor,
      Cyberware.cyberclaws,
      Bioware.eelware,
      Bioware.muscleAugmentation,
      Bioware.neurachem));

  val essentialEnhancements = GearPackage(
    label = "Essential Enhancements",
    cpCost = 1,
    requirements = List(morphType(MorphType.Biomorph, MorphType.Pod, MorphType.Synthmorph)),
    augmentations = List(
      Bioware.enhancedHearing,
      Bioware.enhancedVision,
      Nanoware.medichines,
      Cyberware.tRayEmitter));

  val essentialGearCreepy = GearPackage(
    label = "Essential Gear with Creepy",
    cpCost = 3,
    requirements = List(kineticWeapons >= 20),
    gear = List(
      Bots.creepy,
      EverydayTech.ecto,
      Nanotechnology.maker,
      EverydayTech.smartClothing,
      EverydayTech.utilitool),
    armour = List(
      ArmourEP.bodyArmourLight,
      ArmourEP.vacsuitStandard),
    weapons = List(KineticWeapons.mediumPistol));

  val essentialGearServitor = GearPackage(
    label = "Essential Gear with Servitor",
    cpCost = 3,
    requirements = List(kineticWeapons >= 20),
    gear = List(
      Bots.servitor,
      EverydayTech.ecto,
      Nanotechnology.maker,
      EverydayTech.smartClothing,
      EverydayTech.utilitool),
    armour = List(
      ArmourEP.bodyArmourLight,
      ArmourEP.vacsuitStandard),
    weapons = List(KineticWeapons.mediumPistol));

  val explorer = GearPackage(
    label = "Explorer",
    cpCost = 3,
    requirements = List(beamWeapons >= 20),
    gear = List(Nanotechnology.fabber),
    armour = List(ArmourEP.smartVacClothing),
    weapons = List(
      BeamWeapons.agonizer,
      BeamWeapons.agonizerRoast));

  val explorerSurvival = GearPackage(
    label = "Explorer Survival Variant",
    cpCost = 4,
    requirements = List(
      beamWeapons >= 20,
      blades >= 20,
      pilot.withField("Aircraft") >= 10),
    gear = List(
      Nanotechnology.fabber,
      SurvivalGear.breadcrumbPositioningSystem,
      SurvivalGear.electronicsRope,
      SurvivalGear.emergencyRations,
      SurvivalGear.filterStraw,
      SurvivalGear.flashlight,
      SurvivalGear.nanobandage * 2,
      EverydayTech.solarRecharger,
      CommunicationsGear.radioBooster,
      Bots.reconFlyer,
      SurvivalGear.repairSpray,
      EverydayTech.utilitool,
      EverydayTech.viewers),
    armour = List(ArmourEP.smartVacClothing),
    weapons = List(
      BeamWeapons.agonizer,
      BeamWeapons.agonizerRoast,
      Blades.flexCutter));

  val firewallAgent = GearPackage(
    label = "Firewall Agent",
    cpCost = 18,
    requirements = List(
      kineticWeapons >= 30),
    gear = List(
      Services.anonymousAccounts,
      Services.backup,
      Nanotechnology.fabber,
      Services.fakeEgoID,
      Nanotechnology.guardianSwarm,
      CommunicationsGear.lowCapQBReservoir,
      Nanotechnology.nanodetector,
      CommunicationsGear.portableQEComm),
    armour = List(ArmourEP.bodyArmourLight),
    weapons = List(KineticWeapons.submachineGun.load(KineticAmmo.ap).get),
    software = List(
      SoftwareEP.encryption,
      SoftwareEP.tacticalNetworks));

  val gatecrasher = GearPackage(
    label = "Gatecrasher",
    cpCost = 20,
    requirements = List(
      pilot.oneOf("Anthropod", "Groundcraft") >= 20),
    gear = List(
      SurvivalGear.biodefenseUnit,
      SurvivalGear.defensiveBeacons,
      ExplorationGear.mappingMissile,
      CommunicationsGear.missionRecorder,
      OtherTodo.mobileBase,
      CommunicationsGear.motes, // actually Mote Grenade (Gatecrashing p. 154)
      CommunicationsGear.radioBeacon,
      Bots.robomule,
      CommunicationsGear.satnetInACan,
      ExplorationGear.scoutMissile,
      SurvivalGear.shelterDome));

  val hacker = GearPackage(
    label = "Hacker",
    cpCost = 10,
    requirements = List(
      infosec >= 30,
      hardware.withField("Electronics") >= 20),
    gear = List(
      Services.anonymousAccounts,
      EverydayTech.toolKit.copy(name = "Electronics Tool Kit"),
      Bots.gnat * 2,
      CommunicationsGear.radioBooster),
    software = List(
      SoftwareEP.exploit,
      SoftwareEP.sniffer,
      SoftwareEP.spoof,
      SoftwareEP.tracking));

  val heavyWeapons = GearPackage(
    label = "Heavy Weapons",
    cpCost = 18,
    requirements = List(
      kineticWeapons >= 30,
      beamWeapons >= 30,
      seekerWeapons >= 30),
    armour = List(
      ArmourEP.bodyArmourHeavy.withMod(ArmourMods.fireproofing), // and ablative patches
      ArmourEP.helmetFull),
    weapons = List(
      KineticWeapons.machineGun.load(KineticAmmo.regular).get,
      KineticWeapons.machineGun.load(KineticAmmo.ap).get,
      BeamWeapons.bolter,
      Seekers.seekerRifleMini.load(Missiles.heap(MissileSize.Minimissile)).get));

  val infiltrator = GearPackage(
    label = "Infiltrator",
    cpCost = 15,
    requirements = List(
      infiltration >= 30,
      hardware.withField("Electronics") >= 20),
    gear = List(
      Nanotechnology.cleanerSwarm,
      CovertTech.cot,
      CovertTech.dazzler,
      EverydayTech.toolKit.copy(name = "Electronics Tool Kit"),
      CovertTech.invisibilityCloak,
      Bots.speck * 2),
    armour = List(ArmourEP.armourClothing.withMod(ArmourMods.thermalDampening)));

  val research = GearPackage(
    label = "Research",
    cpCost = 3,
    requirements = List(academics.anyField(-1) >= 30),
    gear = List(
      ScavengerTech.mobileLab,
      EverydayTech.portableSensor,
      Bots.servitor,
      ScavengerTech.specimenContainer,
      EverydayTech.utilitool,
      EverydayTech.viewers));

  val researchExtra = GearPackage(
    label = "Research Extra",
    cpCost = 6,
    requirements = List(
      academics.anyField(-1) >= 30,
      pilot.anyField(-1) >= 20),
    gear = List(
      ScavengerTech.mobileLab,
      EverydayTech.portableSensor,
      Bots.servitor,
      ScavengerTech.specimenContainer,
      EverydayTech.utilitool,
      EverydayTech.viewers,
      Bots.explorenaut,
      XenoarcheologyGear.faradayContainer,
      ExplorationGear.portableSolarchive,
      Bots.robomule,
      XenoarcheologyGear.scourers));

  val scavenger = GearPackage(
    label = "Scavenger",
    cpCost = 8,
    requirements = List(demolitions >= 10),
    gear = List(
      ScavengerTech.disassemblyTools,
      ChemicalsEP.scrapersGel * 4,
      ScavengerTech.superthermiteCharges),
    armour = List(ArmourEP.vacsuitStandard));

  val security = GearPackage(
    label = "Security",
    cpCost = 6,
    requirements = List(
      clubs >= 20,
      throwingWeapons >= 20,
      sprayWeapons >= 20),
    gear = List(
      CovertTech.cuffband,
      CovertTech.prisonerMask),
    armour = List(
      ArmourEP.bodyArmourHeavy.withMod(ArmourMods.offensiveArmor),
      ArmourEP.riotShield),
    weapons = List(
      Grenades.gas(WeaponType.StandardGrenade), // CR 6 and Smoke 4
      SprayWeapons.freezer,
      Clubs.shockBaton));

  val securityExtra = GearPackage(
    label = "Security Extra",
    cpCost = 8,
    requirements = List(
      clubs >= 20,
      throwingWeapons >= 20,
      sprayWeapons >= 20),
    gear = List(
      CovertTech.cuffband,
      CovertTech.prisonerMask,
      Sensors.brainprintScanner,
      Drugs.grin * 3,
      Sensors.idScanner),
    armour = List(
      ArmourEP.bodyArmourHeavy.withMod(ArmourMods.offensiveArmor),
      ArmourEP.riotShield),
    weapons = List(
      Grenades.gas(WeaponType.StandardGrenade), // CR 6 and Smoke 4
      SprayWeapons.freezer,
      Clubs.shockBaton));

  val sensoryMorph = GearPackage(
    label = "Sensory Morph",
    cpCost = 3,
    requirements = List(
      morphType(MorphType.Biomorph, MorphType.Pod),
      perception >= 20),
    augmentations = List(
      Cyberware.antiGlare,
      Bioware.directionSense,
      Bioware.echolocation,
      Bioware.enhancedHearing,
      Bioware.enhancedSmell,
      Bioware.enhancedVision,
      Nanoware.oracles,
      Cyberware.senseFilter,
      Cyberware.tRayEmitter));

  val sensoryMorphSynth = GearPackage(
    label = "Sensory Morph Synth",
    cpCost = 2,
    requirements = List(
      morphType(MorphType.Synthmorph),
      perception >= 20),
    augmentations = List(
      RoboticEnhancements.vision360,
      RoboticEnhancements.chemicalSniffer,
      Cyberware.electricalSense,
      RoboticEnhancements.lidar,
      RoboticEnhancements.radar));

  val selfDefence = GearPackage(
    label = "Self-Defence",
    cpCost = 5,
    requirements = List(
      blades >= 20,
      sprayWeapons >= 20,
      unarmedCombat >= 20),
    armour = List(ArmourEP.armourClothing),
    weapons = List(
      SprayWeapons.shardPistol,
      Unarmed.shockGloves,
      Blades.vibroblade));

  val smartAnimalHandler = GearPackage(
    label = "Smart Animal Handler",
    cpCost = 15,
    requirements = List(
      animalHandling >= 30),
    gear = List(
      Bots.caretaker * 2,
      Pets.guardDog * 2,
      Pets.smartHawk,
      Pets.smartRacoon,
      Pets.smartRat * 2,
      Pets.spaceRoach * 2));

  val socialManipulatorMorph = GearPackage(
    label = "Social Manipulator Morph",
    cpCost = 7,
    requirements = List(
      morphType(MorphType.Biomorph, MorphType.Pod),
      deception >= 20,
      persuasion >= 20),
    augmentations = List(
      Bioware.cleanMetabolism,
      Bioware.enhancedPheromones,
      Bioware.endocrineControl));

  val stealthMorph = GearPackage(
    label = "Stealth Morph",
    cpCost = 2,
    requirements = List(
      morphType(MorphType.Biomorph, MorphType.Pod),
      infiltration >= 30),
    augmentations = List(
      Bioware.chameleonSkin,
      Bioware.enhancedHearing,
      Bioware.enhancedVision,
      Bioware.gripPads,
      Nanoware.skinflex));

  val surveillance = GearPackage(
    label = "Surveillance",
    cpCost = 7,
    requirements = List(
      investigation >= 20),
    gear = List(
      CovertTech.fiberEye,
      CovertTech.smartDust,
      Nanotechnology.scoutSwarm,
      Bots.speck * 2),
    software = List(SoftwareEP.fiRecognition));

  val survivalMorph = GearPackage(
    label = "Survival Morph",
    cpCost = 2,
    requirements = List(morphType(MorphType.Biomorph, MorphType.Pod)),
    augmentations = List(
      Bioware.directionSense,
      Bioware.enhancedRespiration,
      Nanoware.medichines,
      Bioware.temperatureTolerance,
      Bioware.toxinFilters));

  val techie = GearPackage(
    label = "Techie",
    cpCost = 3,
    requirements = List(
      morphType(MorphType.Biomorph, MorphType.Pod, MorphType.Synthmorph),
      hardware.anyField(-1) >= 30),
    gear = List(
      EverydayTech.utilitool,
      EverydayTech.toolKit,
      Nanotechnology.fabber,
      SurvivalGear.repairSpray),
    augmentations = List(
      Cyberware.electricalSense,
      Nanoware.wristMountedTools));

  val list: List[GearPackage] = Macros.memberList[GearPackage];
}

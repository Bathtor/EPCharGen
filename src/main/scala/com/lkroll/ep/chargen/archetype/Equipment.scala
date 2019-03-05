package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._
import com.lkroll.common.macros.Macros

import probability_monad.{ Distribution, Distributions }
import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable

import scala.language.implicitConversions

case class Equipment(
  creditCost:    Int,
  gear:          List[GearEntry]                      = Nil,
  armour:        List[Either[Armour, ModdedArmour]]   = Nil,
  weapons:       List[Either[Weapon, WeaponWithAmmo]] = Nil,
  augmentations: List[Augmentation]                   = Nil,
  software:      List[Software]                       = Nil) {

  def applyTo(c: CharGenCharacter, rand: Random): CharGenCharacter = {
    val newEnhancements = c.activeMorph.enhancements ++ augmentations.map(_.name).toSet.toList;
    val moddedMorph = c.activeMorph.copy(
      enhancements = newEnhancements);
    c.copy(
      activeMorph = moddedMorph,
      gear = mergeGear(c.gear, gear),
      armour = c.armour ++ armour,
      weapons = c.weapons ++ weapons,
      software = c.software ++ software)
  }

  private def mergeGear(left: List[GearEntry], right: List[GearEntry]): List[GearEntry] = {
    val gMap = mutable.Map.empty[String, GearEntry];
    left.foreach { g =>
      gMap.get(g.item.name) match {
        case Some(e) => {
          val newG = e.copy(count = e.count + g.count);
          gMap += (e.item.name -> newG)
        }
        case None => {
          gMap += (g.item.name -> g)
        }
      }
    }
    right.foreach { g =>
      gMap.get(g.item.name) match {
        case Some(e) => {
          val newG = e.copy(count = e.count + g.count);
          gMap += (e.item.name -> newG)
        }
        case None => {
          gMap += (g.item.name -> g)
        }
      }
    }

    gMap.map(_._2).toList
  }
}

class EquipmentSelection(
  val archetype:   Archetype,
  val char:        CharGenCharacter,
  val creditLimit: Int) extends Table with StrictLogging {
  import EquipmentSelection._;

  override type Result = Equipment;

  override def label: String = "Archetype Equipment Selection";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    var creditCost: Int = 0;
    var gear: List[GearEntry] = Nil;
    var armour: List[Either[Armour, ModdedArmour]] = Nil;
    var weapons: List[Either[Weapon, WeaponWithAmmo]] = Nil;
    var augmentations: List[Augmentation] = Nil;
    var software: List[Software] = Nil;

    val priority = priorityData.randomElement(rand).get;
    val (restCredit, creditPriority) = priority.foldLeft((creditLimit, List.empty[(EquipmentPriority, Int)])) { (acc, p) =>
      val (remainingCredit, assigned) = acc;
      val max = remainingCredit / 2;
      val min = remainingCredit / 5;
      val top = max - min;
      val limit = rand.nextInt(top) + min;
      val rem = remainingCredit - limit;
      (rem, ((p, limit) :: assigned))
    };
    creditPriority.foreach { t =>
      logger.debug(s"Starting priority item $t");
      val (p, limit) = t;
      var creditSpent = 0;
      var firstArmour = true;
      var retry = 0;
      val retryLimit = 10;

      val pick: () => Unit = p match {
        case EquipmentPriority.Weapons => () => {
          val res = pickWeapon(rand);
          val existing = weapons.filter(w => w.matches(res));
          if (existing.size < 2) {
            val price = res.price;
            creditSpent += price;
            creditCost += price;
            weapons ::= res;
          } else {
            logger.trace(s"Ignoring triplicate weapon $res");
            retry += 1;
          }
        }
        case EquipmentPriority.Armour => () => {
          val res = pickArmour(rand, firstArmour);
          firstArmour = false;
          if (armour.find(a => a.matches(res)).isEmpty) {
            retry = 0;
            val price = res.price;
            creditSpent += price;
            creditCost += price;
            armour ::= res;
          } else {
            logger.trace(s"Ignoring duplicate armour $res");
            retry += 1;
          }
        }
        case EquipmentPriority.Augmentations => () => {
          val res = pickAugmentation(rand);
          if (augmentations.find(a => a.name == res.name).isEmpty) {
            retry = 0;
            val price = res.price.average;
            creditSpent += price;
            creditCost += price;
            augmentations ::= res;
          } else {
            logger.trace(s"Ignoring duplicate augmentation $res");
            retry += 1;
          }
        }
        case EquipmentPriority.Gear => () => {
          val res = pickGear(rand);
          val (existing, rest) = gear.partition(g => g.item.name == res.item.name);
          val price = res.item.price.average * res.count;
          creditSpent += price;
          creditCost += price;
          if (existing.isEmpty) {
            gear ::= res;
          } else {
            gear = GearEntry(res.item, res.count + existing.head.count) :: rest;
          }
        }
        case EquipmentPriority.Software => () => {
          val res = pickSoftware(rand);
          if (software.find(a => a.name == res.name).isEmpty) {
            retry = 0;
            val price = res.price.average;
            creditSpent += price;
            creditCost += price;
            software ::= res;
          } else {
            logger.trace(s"Ignoring duplicate software $res");
            retry += 1;
          }
        }
      };

      while (creditSpent < limit && creditCost < creditLimit && retry < retryLimit) {
        logger.debug(s"Next selection round with creditSpent=${creditSpent}, creditCost=${creditCost}, and retry=${retry}");
        pick();
      }
    }

    Equipment(creditCost, gear, armour, weapons, augmentations, software)
  }

  private def pickWeapon(rand: Random): Either[Weapon, WeaponWithAmmo] = {
    val weaponSkills = char.skills.filter(s =>
      (s.skillDef.category == SkillCategory.Combat) &&
        (s.name != DefaultSkills.fray.name) &&
        (s.name != DefaultSkills.gunnery.name));
    val existingSkills = weaponSkills.filter(s => s.ranks > 0);
    if (!existingSkills.isEmpty) {
      logger.trace(s"Character has some useful weapon skills: ${existingSkills.map(_.name).mkString(", ")}");
      val (goodSkills, badSkills) = existingSkills.partition(s => s.ranks >= 30);
      val dist = new Distributions(rand);
      val geom = dist.geometric(0.7);
      val pickGood = rand.nextInt(100) < 80; // 80% probability to pick something you are good with
      val targetSkill = if (goodSkills.isEmpty || (!pickGood && !badSkills.isEmpty)) {
        logger.trace(s"Not picking good skills: ${goodSkills.map(_.name).mkString(", ")}");
        val pos = geom.sampleClamp(1, badSkills.length) - 1; // need 0-indexing
        badSkills(pos)
      } else {
        logger.trace(s"Picking from good skills: ${goodSkills.map(_.name).mkString(", ")}");
        val pos = geom.sampleClamp(1, goodSkills.length) - 1; // need 0-indexing
        goodSkills(pos)
      };
      // pick a weapon for skill
      targetSkill.name match {
        case "Beam Weapons" => {
          val w = beamWeaponsData.randomElement(rand).get;
          Left(w)
        }
        case "Blades" => {
          val w = bladesData.randomElement(rand).get;
          Left(w)
        }
        case "Clubs" => {
          val w = clubsData.randomElement(rand).get;
          Left(w)
        }
        case "Exotic Melee Weapons" => {
          Left(ExoticWeapons.monowireGarrote)
        }
        case "Exotic Ranged Weapons" => {
          Left(ExoticWeapons.vortex)
        }
        case "Kinetic Weapons" => {
          val w = kineticWeaponsData.randomElement(rand).get;
          val railProbability = archetype match {
            case Archetype.Butterfly | Archetype.Scientist => 20
            case Archetype.Fighter | Archetype.Hacker      => 50
          };
          if (rand.nextInt(100) < railProbability) {
            val railgun = w.toRailgun;
            val railAmmo = KineticAmmo.list.filter(a => a.appliesTo.contains(WeaponType.Railgun));
            val pos = rand.nextInt(railAmmo.size);
            val ammo = railAmmo(pos);
            val wwa = railgun.load(ammo).get; // should work
            Right(wwa)
          } else {
            val standardProbability = archetype match {
              case Archetype.Butterfly | Archetype.Scientist => 80
              case Archetype.Fighter | Archetype.Hacker      => 40
            };
            if (rand.nextInt(100) < standardProbability) {
              Left(w) // no point to load regular ammo as it's default anyway
            } else {
              val pos = rand.nextInt(KineticAmmo.list.size);
              val ammo = KineticAmmo.list(pos);
              val wwa = w.load(ammo).get; // should work
              Right(wwa)
            }
          }
        }
        case "Seeker Weapons" => {
          val w = seekerWeaponsData.randomElement(rand).get;
          val ammoList = w.`type` match {
            case WeaponType.Seeker(MissileSize.Micromissile) => Missiles.micro
            case WeaponType.Seeker(MissileSize.Minimissile) => Missiles.mini
            case WeaponType.Seeker(MissileSize.StandardMissile) => Missiles.standard
            case _ => ??? // better be a seeker weapon!
          };
          val pos = rand.nextInt(ammoList.size);
          val ammo = ammoList(pos);
          val wwa = w.load(ammo).get; // should work
          Right(wwa)
        }
        case "Spray Weapons" => {
          val w = sprayWeaponsData.randomElement(rand).get;
          Left(w)
        }
        case "Throwing Weapons" => {
          val microProbability = archetype match {
            case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => 90
            case Archetype.Fighter => 50
          };
          if (rand.nextInt(100) < microProbability) {
            val pos = rand.nextInt(Grenades.micro.size);
            Left(Grenades.micro(pos))
          } else {
            val pos = rand.nextInt(Grenades.standard.size);
            Left(Grenades.standard(pos))
          }
        }
        case "Unarmed Combat" => {
          val w = unarmedData.randomElement(rand).get;
          Left(w)
        }
      }
    } else {
      logger.trace("Picking a very simple weapons");
      // pick a VERY simple weapon
      val w = verySimpleWeapons.randomElement(rand).get;
      Left(w)
    }
  }

  private def pickArmour(rand: Random, firstArmour: Boolean): Either[Armour, ModdedArmour] = {
    if (firstArmour) {
      val res = fullArmourData.randomElement(rand).get;
      Left(res) // no mods for now
    } else {
      val res = armourAccessoryData.randomElement(rand).get;
      Left(res)
    }
  }

  private def pickAugmentation(rand: Random): Augmentation = {
    var itemO: Option[Augmentation] = None;
    while (itemO.isEmpty) {
      val gearType = gearTypeData.randomElement(rand).get;
      val applicable = gearType.augmentationList.filter(a => a.appliesTo.contains(char.activeMorph.morphType));
      val list = applicable.filter(a => !char.activeMorph.enhancements.contains(a.name));
      if (!list.isEmpty) {
        val pos = rand.nextInt(list.size);
        itemO = Some(list(pos));
      }
    }
    itemO.get
  }

  private def pickGear(rand: Random): GearEntry = {
    var itemO: Option[GearEntry] = None;
    while (itemO.isEmpty) {
      val gearType = gearTypeData.randomElement(rand).get;
      val list = gearType.gearList;
      if (!list.isEmpty) {
        val pos = rand.nextInt(list.size);
        val item = list(pos);
        itemO = Some(GearEntry(item, 1))
      }
    }
    itemO.get
  }

  private def pickSoftware(rand: Random): Software = {
    softwareData.randomElement(rand).get
  }

  private val priorityData: RollTable[List[EquipmentPriority]] = {
    import EquipmentPriority._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 50) -> List(Augmentations, Gear, Weapons, Software),
        (51 to 90) -> List(Gear, Augmentations, Weapons, Armour, Software),
        (91 to 95) -> List(Gear, Weapons, Armour, Software),
        (96 to 100) -> List(Weapons, Gear, Armour, Software))
      case Archetype.Fighter => RollTable(
        (1 to 30) -> List(Weapons, Armour, Augmentations, Gear, Software),
        (31 to 60) -> List(Armour, Weapons, Augmentations, Gear, Software),
        (61 to 80) -> List(Augmentations, Weapons, Armour, Gear, Software),
        (81 to 90) -> List(Augmentations, Armour, Weapons, Gear, Software),
        (91 to 95) -> List(Armour, Weapons, Gear, Software),
        (96 to 100) -> List(Weapons, Armour, Gear, Software))
      case Archetype.Hacker => RollTable(
        (1 to 30) -> List(Software, Gear, Augmentations, Armour, Weapons),
        (31 to 60) -> List(Gear, Software, Augmentations, Armour, Weapons),
        (61 to 80) -> List(Augmentations, Gear, Software, Armour, Weapons),
        (81 to 90) -> List(Augmentations, Software, Gear, Armour, Weapons),
        (91 to 95) -> List(Software, Gear, Armour, Weapons),
        (96 to 100) -> List(Gear, Software, Armour, Weapons))
      case Archetype.Scientist => RollTable(
        (1 to 30) -> List(Gear, Augmentations, Software, Armour, Weapons),
        (31 to 60) -> List(Augmentations, Gear, Software, Armour, Weapons),
        (61 to 80) -> List(Gear, Augmentations, Armour, Weapons, Software),
        (81 to 90) -> List(Augmentations, Gear, Armour, Weapons, Software),
        (91 to 95) -> List(Gear, Software, Armour, Weapons),
        (96 to 100) -> List(Gear, Weapons, Armour, Software))
    }
  };

  private lazy val beamWeaponsData: RollTable[Weapon] = {
    import BeamWeapons._;
    archetype match {
      case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 33) -> laserPulser,
        (34 to 66) -> agonizer,
        (67 to 100) -> stunner)
      case Archetype.Fighter => RollTable(
        (1 to 30) -> laserPulser,
        (31 to 50) -> agonizer,
        (61 to 70) -> bolter,
        (71 to 80) -> plasmaRifle,
        (81 to 100) -> stunner)
    }
  }

  private lazy val bladesData: RollTable[Weapon] = {
    import Blades._;
    archetype match {
      case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 10) -> diamondAx,
        (11 to 20) -> flexCutter,
        (21 to 80) -> knife,
        (81 to 100) -> vibroblade)
      case Archetype.Fighter => RollTable(
        (1 to 14) -> diamondAx,
        (15 to 30) -> flexCutter,
        (31 to 45) -> knife,
        (46 to 50) -> monofilamentSword,
        (51 to 65) -> vibroblade,
        (66 to 95) -> waspKnife,
        (96 to 100) -> plasmaBlade)
    }
  }

  private lazy val clubsData: RollTable[Weapon] = {
    import Clubs._;
    archetype match {
      case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 20) -> club,
        (21 to 80) -> extendableBaton,
        (81 to 100) -> shockBaton)
      case Archetype.Fighter => RollTable(
        (1 to 20) -> club,
        (21 to 60) -> extendableBaton,
        (61 to 100) -> shockBaton)
    }
  }

  private lazy val kineticWeaponsData: RollTable[Weapon] = {
    import KineticWeapons._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 90) -> lightPistol,
        (91 to 100) -> mediumPistol)
      case Archetype.Fighter => RollTable(
        (1 to 10) -> lightPistol,
        (11 to 25) -> mediumPistol,
        (26 to 30) -> heavyPistol,
        (31 to 60) -> submachineGun,
        (61 to 75) -> auomaticRifle,
        (76 to 85) -> sniperRifle,
        (86 to 100) -> machineGun)
      case Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 70) -> lightPistol,
        (71 to 80) -> mediumPistol,
        (81 to 85) -> heavyPistol,
        (86 to 100) -> submachineGun)
    }
  }

  private lazy val seekerWeaponsData: RollTable[Weapon] = {
    import Seekers._;
    archetype match {
      case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 50) -> seekerArmband,
        (51 to 100) -> seekerPistol)
      case Archetype.Fighter => RollTable(
        (1 to 15) -> disposableLauncher,
        (16 to 35) -> seekerArmband,
        (36 to 55) -> seekerPistol,
        (56 to 65) -> seekerRifleMicro,
        (66 to 75) -> seekerRifleMini,
        (76 to 100) -> underbarrelSeeker)
    }
  }

  private lazy val sprayWeaponsData: RollTable[Weapon] = {
    import SprayWeapons._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 15) -> buzzer,
        (16 to 30) -> freezer,
        (31 to 85) -> shardPistol,
        (86 to 100) -> sprayer)
      case Archetype.Fighter => RollTable(
        (1 to 10) -> buzzer,
        (11 to 20) -> freezer,
        (21 to 50) -> shardPistol,
        (51 to 75) -> shredder,
        (76 to 85) -> sprayer,
        (86 to 100) -> torch)
      case Archetype.Hacker => RollTable(
        (1 to 50) -> buzzer,
        (51 to 70) -> freezer,
        (71 to 90) -> shardPistol,
        (91 to 100) -> sprayer)
      case Archetype.Scientist => RollTable(
        (1 to 30) -> buzzer,
        (31 to 60) -> freezer,
        (61 to 70) -> shardPistol,
        (71 to 100) -> sprayer)
    }
  }

  private lazy val unarmedData: RollTable[Weapon] = {
    import Unarmed._;
    archetype match {
      case Archetype.Butterfly | Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 50) -> densiplastGloves,
        (51 to 100) -> shockGloves)
      case Archetype.Fighter => RollTable(
        (1 to 30) -> densiplastGloves,
        (31 to 100) -> shockGloves)
    }
  }

  private lazy val fullArmourData: RollTable[Armour] = {
    import ArmourEP._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 50) -> armourClothing,
        (51 to 60) -> bodyArmourLight,
        (61 to 80) -> smartVacClothing,
        (81 to 90) -> vacsuitLight,
        (91 to 100) -> vacsuitLightSmart)
      case Archetype.Fighter => RollTable(
        (1 to 5) -> armourClothing,
        (6 to 35) -> bodyArmourLight,
        (36 to 50) -> bodyArmourHeavy,
        (51 to 55) -> crashSuitI,
        (56 to 50) -> smartVacClothing,
        (61 to 65) -> vacsuitLight,
        (66 to 70) -> vacsuitLightSmart,
        (71 to 75) -> vacsuitStandard,
        (76 to 80) -> vacsuitStandardSmart,
        (81 to 90) -> hardsuit,
        (91 to 100) -> crasherSuit)
      case Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 20) -> armourClothing,
        (21 to 30) -> bodyArmourLight,
        (31 to 5) -> crashSuitI,
        (36 to 40) -> smartVacClothing,
        (45 to 55) -> vacsuitLight,
        (56 to 65) -> vacsuitLightSmart,
        (66 to 75) -> vacsuitStandard,
        (76 to 85) -> vacsuitStandardSmart,
        (86 to 100) -> crasherSuit)
    }
  }

  private lazy val armourAccessoryData: RollTable[Armour] = {
    import ArmourEP._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 10) -> armourVest,
        (11 to 70) -> secondSkin,
        (71 to 100) -> smartSkin)
      case Archetype.Fighter => RollTable(
        (1 to 25) -> armourVest,
        (26 to 40) -> helmetLight,
        (41 to 55) -> helmetFull,
        (56 to 65) -> riotShield,
        (66 to 85) -> secondSkin,
        (86 to 100) -> smartSkin)
      case Archetype.Hacker | Archetype.Scientist => RollTable(
        (1 to 30) -> armourVest,
        (31 to 50) -> helmetLight,
        (51 to 75) -> secondSkin,
        (76 to 100) -> smartSkin)
    }
  }

  private lazy val softwareData: RollTable[Software] = {
    import SoftwareEP._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 10) -> arIllusions,
        (11 to 25) -> encryption,
        (26 to 40) -> fiRecognition,
        (41 to 55) -> firewall,
        (56 to 65) -> tacticalNetworks,
        (66 to 80) -> tracking,
        (81 to 100) -> xp)
      case Archetype.Fighter => RollTable(
        (1 to 20) -> encryption,
        (21 to 30) -> fiRecognition,
        (31 to 50) -> firewall,
        (51 to 80) -> tacticalNetworks,
        (81 to 100) -> tracking)
      case Archetype.Hacker => RollTable(
        (1 to 10) -> arIllusions,
        (11 to 20) -> encryption,
        (21 to 40) -> exploit,
        (41 to 45) -> fiRecognition,
        (46 to 55) -> firewall,
        (56 to 65) -> sniffer,
        (66 to 75) -> spoof,
        (76 to 85) -> tacticalNetworks,
        (86 to 95) -> tracking,
        (96 to 100) -> xp)
      case Archetype.Scientist => RollTable(
        (1 to 25) -> encryption,
        (26 to 50) -> firewall,
        (51 to 80) -> tacticalNetworks,
        (81 to 100) -> xp)
    }
  }

  private lazy val gearTypeData: RollTable[GearPurpose] = {
    import GearPurpose._;
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 10) -> Combat,
        (11 to 25) -> Perception,
        (26 to 40) -> Infiltration,
        (41 to 70) -> Social,
        (71 to 75) -> Tech,
        (76 to 100) -> General)
      case Archetype.Fighter => RollTable(
        (1 to 30) -> Combat,
        (31 to 40) -> Perception,
        (41 to 60) -> Infiltration,
        (61 to 70) -> Tech,
        (71 to 100) -> General)
      case Archetype.Hacker => RollTable(
        (1 to 10) -> Combat,
        (11 to 30) -> Perception,
        (31 to 45) -> Infiltration,
        (46 to 50) -> Social,
        (51 to 80) -> Tech,
        (81 to 100) -> General)
      case Archetype.Scientist => RollTable(
        (1 to 5) -> Combat,
        (6 to 25) -> Perception,
        (26 to 35) -> Infiltration,
        (36 to 40) -> Social,
        (41 to 65) -> Tech,
        (66 to 100) -> General)
    }
  }
}

object EquipmentSelection {
  sealed trait EquipmentPriority;
  object EquipmentPriority {
    case object Weapons extends EquipmentPriority;
    case object Armour extends EquipmentPriority;
    case object Augmentations extends EquipmentPriority;
    case object Software extends EquipmentPriority;
    case object Gear extends EquipmentPriority;
  }

  implicit class WeaponExt(e: Either[Weapon, WeaponWithAmmo]) {
    def price: Int = e match {
      case Left(w)    => w.price.average
      case Right(wwa) => wwa.weapon.price.average + wwa.ammo.price.average
    }
    def matches(other: Either[Weapon, WeaponWithAmmo]): Boolean = {
      this.name == other.name
    }
    def name: String = e match {
      case Left(a)   => a.name
      case Right(ma) => ma.name
    }
  }
  implicit class ArmourExt(e: Either[Armour, ModdedArmour]) {
    def price: Int = e match {
      case Left(a)   => a.price.average
      case Right(ma) => ma.baseArmour.price.average + ma.mod.price.average
    }
    def matches(other: Either[Armour, ModdedArmour]): Boolean = {
      (e, other) match {
        case (Left(a1), Left(a2))     => a1.name == a2.name
        case (Right(ma1), Right(ma2)) => (ma1.baseArmour.name == ma2.baseArmour.name) && (ma1.mod.name == ma2.mod.name)
        case _                        => false
      }
    }
    def name: String = e match {
      case Left(a)   => a.name
      case Right(ma) => ma.name
    }
  }

  val verySimpleWeapons: RollTable[Weapon] = RollTable(
    (1 to 30) -> Blades.knife,
    (31 to 60) -> KineticWeapons.lightPistol,
    (61 to 70) -> Clubs.extendableBaton,
    (71 to 90) -> BeamWeapons.agonizer,
    (91 to 100) -> SprayWeapons.shardPistol);

  sealed trait GearPurpose {
    def gearList: List[Gear];
    def augmentationList: List[Augmentation];
  }
  object GearPurpose {
    case object Combat extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CovertTech.disabler);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.adrenalBoost,
        Bioware.lightBioweaveArmor,
        Bioware.heavyBioweaveArmor,
        Bioware.carapaceArmor,
        Bioware.claws,
        Bioware.eelware,
        Bioware.endocrineControl,
        Bioware.muscleAugmentation,
        Bioware.neurachem,
        Bioware.poisonGland,
        Cyberware.cyberclaws,
        Cyberware.cyberlimbPlus,
        Cyberware.handLaser,
        Cyberware.hardenedSkeleton,
        Cyberware.reflexBoosters,
        Nanoware.implantedNanotoxins,
        RoboticEnhancements.heavyCombatArmour,
        RoboticEnhancements.lightCombatArmour,
        RoboticEnhancements.pneumaticLimbs,
        RoboticEnhancements.structuralEnhancement);
    }
    case object Perception extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CovertTech.fiberEye,
        CovertTech.microbug,
        CovertTech.microbugQF,
        CovertTech.smartDust,
        CovertTech.xrayEmitter,
        CovertTech.acousticSpotter,
        EverydayTech.portableSensor,
        EverydayTech.specs,
        EverydayTech.viewers,
        Nanotechnology.nanodetector,
        Nanotechnology.scoutSwarm,
        Nanotechnology.taggantSwarm);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.directionSense,
        Bioware.echolocation,
        Bioware.enhancedHearing,
        Bioware.enhancedSmell,
        Bioware.enhancedVision,
        Bioware.polarizationVision,
        Bioware.ultravioletVision,
        Bioware.lateralLine,
        Cyberware.antiGlare,
        Cyberware.electricalSense,
        Cyberware.radiationSense,
        Cyberware.tRayEmitter,
        Cyberware.senseFilter,
        Nanoware.oracles,
        RoboticEnhancements.vision360,
        RoboticEnhancements.chemicalSniffer,
        RoboticEnhancements.lidar,
        RoboticEnhancements.nanoVision,
        RoboticEnhancements.radar);
    }
    case object Infiltration extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CovertTech.chameleonCloak,
        CovertTech.cot,
        CovertTech.dazzler,
        CovertTech.invisibilityCloak,
        CovertTech.whiteNoiseMachine,
        CovertTech.dnaChaff,
        CovertTech.lensCrazer,
        CovertTech.lensSpotter,
        CovertTech.radarCloak,
        CovertTech.shroud,
        Nanotechnology.nanomask);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.chameleonSkin,
        Bioware.emotionalDampers,
        Bioware.endocrineControl,
        Bioware.skinPocket,
        Cyberware.deadSwitch,
        Cyberware.failsafe,
        Cyberware.implantMasking,
        Cyberware.memoryLock,
        Nanoware.skinflex,
        Nanoware.gaitMasking,
        Nanoware.nanotatIdFlux,
        Nanoware.skeletalDisguise,
        RoboticEnhancements.hiddenCompartment,
        RoboticEnhancements.invisibility,
        RoboticEnhancements.radarAbsorbent,
        RoboticEnhancements.reducedSignature,
        RoboticEnhancements.radarInvsibility);
    }
    case object Social extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CovertTech.shroud);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.emotionalDampers,
        Bioware.endocrineControl,
        Bioware.enhancedPheromones,
        Cyberware.truthFilters);
    }
    case object Tech extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CommunicationsGear.fiberopticCable,
        CommunicationsGear.laserLink,
        CommunicationsGear.radioBooster,
        CommunicationsGear.minRadioFarcaster,
        CommunicationsGear.portableQEComm,
        CommunicationsGear.missionRecorder,
        CommunicationsGear.motes,
        CommunicationsGear.hypersonicCommunicator,
        CovertTech.cot,
        CovertTech.whiteNoiseMachine,
        CovertTech.greyBox,
        EverydayTech.ecto,
        EverydayTech.toolKit,
        EverydayTech.utilitool,
        EverydayTech.fractalGloves,
        EverydayTech.smartManipulators,
        Nanotechnology.generalHive,
        Nanotechnology.specialisedHive,
        Nanotechnology.disassemblerSwarm,
        Nanotechnology.engineerSwarm,
        Nanotechnology.fixerSwarm,
        Nanotechnology.injectorSwarm,
        Nanotechnology.guardianSwarm,
        Nanotechnology.saboteurSwarm,
        Nanotechnology.subverbeeSwarm,
        ScavengerTech.disassemblyTools,
        ScavengerTech.mobileLab,
        ScavengerTech.specimenContainer,
        ScavengerTech.superthermiteCharges,
        SurvivalGear.plasmaCutter);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.eelware,
        Cyberware.qecomm,
        Cyberware.parallelProcessor,
        Nanoware.skinlink,
        Nanoware.wristMountedTools,
        Nanoware.personalPowerPlant,
        RoboticEnhancements.fractalDigist);
    }
    case object General extends GearPurpose {
      override lazy val gearList: List[Gear] = List(
        CovertTech.tractionPads,
        EverydayTech.microgravShoes,
        EverydayTech.smartClothing,
        Nanotechnology.fabber,
        Nanotechnology.maker,
        Nanotechnology.cleanerSwarm,
        Nanotechnology.gardenerSwarm,
        SurvivalGear.breadcrumbPositioningSystem,
        SurvivalGear.electrograviticsNet,
        SurvivalGear.electronicsRope,
        SurvivalGear.flashlight,
        SurvivalGear.nanobandage,
        SurvivalGear.repairSpray,
        SurvivalGear.spindle,
        SurvivalGear.spindleClimber,
        SurvivalGear.highdiveSuit);
      override lazy val augmentationList: List[Augmentation] = List(
        Bioware.eideticMemory,
        Bioware.hyperLinguist,
        Bioware.mathBoost,
        Bioware.multiplePersonalities,
        Bioware.circadianRegulation,
        Bioware.cleanMetabolism,
        Bioware.drugGlands,
        Bioware.enhancedRespiration,
        Bioware.gills,
        Bioware.gripPads,
        Bioware.hibernation,
        Bioware.prehensileFeet,
        Bioware.prehensileTail,
        Bioware.sexSwitch,
        Bioware.temperatureTolerance,
        Bioware.toxinFilters,
        Bioware.vacuumSealing,
        Bioware.highGAdaptation,
        Bioware.swimBladder,
        Bioware.hydrostaticPressureAdaptation,
        Bioware.radiationTolerance,
        Bioware.temperatureToleranceCryonic,
        Bioware.temperatureToleranceImpCold,
        Bioware.glidingMembrane,
        Bioware.lowPressureTolerance,
        Bioware.wings,
        Cyberware.accessJacks,
        Cyberware.emergencyFarcaster,
        Cyberware.ghostriderModule,
        Cyberware.mnemonicAugmentation,
        Cyberware.multiTasking,
        Cyberware.puppetSock,
        Cyberware.cyberlimb,
        Cyberware.oxygenReserve,
        Cyberware.lifeRecorder,
        Cyberware.gasJetSystem,
        Nanoware.medichines,
        Nanoware.mentalSpeed,
        Nanoware.nanophages,
        Nanoware.respirocytes,
        Nanoware.longTermLifesupport,
        Nanoware.neuralEnhancers,
        RoboticEnhancements.industrialArmour,
        RoboticEnhancements.extraLimbs,
        RoboticEnhancements.magneticSystem,
        RoboticEnhancements.retractingLimbs,
        RoboticEnhancements.cryonicProtection,
        RoboticEnhancements.extremeHeatShielding,
        RoboticEnhancements.extremePressureAdapation,
        RoboticEnhancements.radiationShielding);
    }
  }
}

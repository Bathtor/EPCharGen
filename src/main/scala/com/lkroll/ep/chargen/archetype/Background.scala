package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.MorphModel
import com.lkroll.ep.compendium.data._
import com.lkroll.common.macros.Macros

case class Background(label: String, group: String, pkg: BackgroundPackage, startingMorph: MorphModel)

sealed trait Origin;
object Origin {
  case object EarthBorn extends Origin;
  case object Orbital extends Origin;
  case object Lunar extends Origin;
  case object Martian extends Origin;
  case object SunwardHab extends Origin;
  case object RimwardHab extends Origin;
  case object Migrant extends Origin;
  case object CreatedNotBorn extends Origin;

  val list: List[Origin] = Macros.memberList[Origin];

  def fromString(s: String): Origin = s.toLowerCase() match {
    case "earthborn"      => EarthBorn
    case "orbital"        => Orbital
    case "lunar"          => Lunar
    case "martian"        => Martian
    case "sunwardhab"     => SunwardHab
    case "rimwardhab"     => RimwardHab
    case "migrant"        => Migrant
    case "creatednotborn" => CreatedNotBorn
  };
}

class BackgroundTable(val archetype: Archetype, val backgroundLevel: PackageLevel, val origin: Option[Origin] = None)
    extends Table {
  import Implicits.constToRollTable;

  override type Result = Background;

  private case class BackgroundClass(label: String, nextTable: Int)
  private object BackgroundClasses {
    def fromOrigin(o: Origin): BackgroundClass = {
      import Origin._;

      o match {
        case EarthBorn      => earthborn
        case Orbital        => orbital
        case Lunar          => lunar
        case Martian        => martian
        case SunwardHab     => sunward
        case RimwardHab     => rimward
        case Migrant        => migrant
        case CreatedNotBorn => created
      }
    }

    val earthborn = BackgroundClass("Earthborn", 2);
    val orbital = BackgroundClass("Orbital", 3);
    val lunar = BackgroundClass("Lunar", 4);
    val martian = BackgroundClass("Martian Settler", 5);
    val sunward = BackgroundClass("Sunward Hab", 6);
    val rimward = BackgroundClass("Rimward Hab", 7);
    val migrant = BackgroundClass("Migrant", 8);
    val created = BackgroundClass("Created not Born", 9);
  }
  sealed private trait BackgroundEntry {
    def resolve(rand: Random, backgroundGroup: String): Background;
  }
  private case class UnresolvedBackground(label: String,
                                          pkg: PackageGroup[BackgroundPackage],
                                          startingMorph: RollTableLike[MorphModel])
      extends BackgroundEntry {
    override def resolve(rand: Random, backgroundGroup: String): Background = {
      Background(label,
                 backgroundGroup,
                 pkg.randomElement(rand).flatMap(_.ofLevel(backgroundLevel)).get,
                 startingMorph.randomElement(rand).get)
    }
  }
  private case class SubTableBackground(label: String, table: RollTable[BackgroundEntry]) extends BackgroundEntry {
    override def resolve(rand: Random, backgroundGroup: String): Background = {
      val bg = table.randomElement(rand).get;
      val b = bg.resolve(rand, backgroundGroup);
      b.copy(label = s"${label} - ${b.label}")
    }
  }

  private val data1: RollTable[BackgroundClass] = RollTable(
    (1 to 50) -> BackgroundClasses.earthborn,
    (51 to 60) -> BackgroundClasses.orbital,
    (61 to 68) -> BackgroundClasses.lunar,
    (69 to 76) -> BackgroundClasses.martian,
    (77 to 82) -> BackgroundClasses.sunward,
    (83 to 89) -> BackgroundClasses.rimward,
    (90 to 95) -> BackgroundClasses.migrant,
    (96 to 100) -> BackgroundClasses.created
  );

  private lazy val data2: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 3) -> UnresolvedBackground("Born with a silver nanoswarm in your blood",
                                           BackgroundPackages.hypereliteScion,
                                           MorphTables.exalt),
          (4 to 9) -> UnresolvedBackground("Celebrity child", BackgroundPackages.hypereliteMedia, MorphTables.sylph),
          (10 to 15) -> UnresolvedBackground("Privileged: enclave born",
                                             BackgroundPackages.fallEvacueeEnclaver,
                                             MorphTables.splicer),
          (16 to 16) -> UnresolvedBackground("Precariat: poverty just a step away",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (17 to 17) -> UnresolvedBackground("Troubled: raised among disaster or war",
                                             BackgroundPackages.fallEvacueeUnderclass,
                                             MorphTables.flat),
          (18 to 18) -> UnresolvedBackground("Raised on the street", BackgroundPackages.streetRat, MorphTables.flat),
          (19 to 20) -> UnresolvedBackground("Raised in a collective/commune grouping",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Born with a silver nanoswarm in your blood",
                                           BackgroundPackages.hypereliteScion,
                                           MorphTables.exalt),
          (2 to 2) -> UnresolvedBackground("Celebrity child", BackgroundPackages.hypereliteMedia, MorphTables.sylph),
          (3 to 6) -> UnresolvedBackground("Privileged: enclave born",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (7 to 9) -> UnresolvedBackground("Precariat: poverty just a step away",
                                           BackgroundPackages.reinstantiatedCivilian,
                                           MorphTables.splicerOrFlat),
          (10 to 15) -> UnresolvedBackground("Troubled: raised among disaster or war",
                                             BackgroundPackages.fallEvacueeUnderclass,
                                             MorphTables.flat),
          (16 to 19) -> UnresolvedBackground("Raised on the street", BackgroundPackages.streetRat, MorphTables.flat),
          (20 to 20) -> UnresolvedBackground("Raised in a collective/commune grouping",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Born with a silver nanoswarm in your blood",
                                           BackgroundPackages.hypereliteScion,
                                           MorphTables.exalt),
          (2 to 2) -> UnresolvedBackground("Celebrity child", BackgroundPackages.hypereliteMedia, MorphTables.sylph),
          (3 to 10) -> UnresolvedBackground("Privileged: enclave born",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (11 to 13) -> UnresolvedBackground("Precariat: poverty just a step away",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (14 to 16) -> UnresolvedBackground("Troubled: raised among disaster or war",
                                             BackgroundPackages.fallEvacueeUnderclass,
                                             MorphTables.flat),
          (17 to 18) -> UnresolvedBackground("Raised on the street", BackgroundPackages.streetRat, MorphTables.flat),
          (19 to 20) -> UnresolvedBackground("Raised in a collective/commune grouping",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 5) -> UnresolvedBackground("Born with a silver nanoswarm in your blood",
                                           BackgroundPackages.hypereliteScion,
                                           MorphTables.exalt),
          (6 to 6) -> UnresolvedBackground("Celebrity child", BackgroundPackages.hypereliteMedia, MorphTables.sylph),
          (7 to 13) -> UnresolvedBackground("Privileged: enclave born",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (14 to 14) -> UnresolvedBackground("Precariat: poverty just a step away",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (15 to 16) -> UnresolvedBackground("Troubled: raised among disaster or war",
                                             BackgroundPackages.fallEvacueeUnderclass,
                                             MorphTables.flat),
          (17 to 17) -> UnresolvedBackground("Raised on the street", BackgroundPackages.streetRat, MorphTables.flat),
          (18 to 20) -> UnresolvedBackground("Raised in a collective/commune grouping",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat)
        );

    }
  }

  private lazy val data3: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 5) -> UnresolvedBackground("Orbital elite", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (6 to 10) -> UnresolvedBackground("A new star born above the Earth",
                                            BackgroundPackages.hypereliteMedia,
                                            MorphTables.sylph),
          (11 to 14) -> UnresolvedBackground("Orbital colonist; floating above the masses",
                                             BackgroundPackages.fallEvacueeEnclaver,
                                             MorphTables.splicer),
          (15 to 17) -> SubTableBackground("Orbital colony staff", colonialStaffTable),
          (18 to 19) -> UnresolvedBackground("One of the lucky few to live above",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.lucky),
          (20 to 20) -> UnresolvedBackground("Orbital worker family",
                                             BackgroundPackages.indenture,
                                             MorphTables.indenture)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 4) -> UnresolvedBackground("Orbital elite", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (5 to 5) -> UnresolvedBackground("A new star born above the Earth",
                                           BackgroundPackages.hypereliteMedia,
                                           MorphTables.sylph),
          (6 to 9) -> UnresolvedBackground("Orbital colonist; floating above the masses",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (10 to 17) -> SubTableBackground("Orbital colony staff", colonialStaffTable),
          (18 to 19) -> UnresolvedBackground("One of the lucky few to live above",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.lucky),
          (20 to 20) -> UnresolvedBackground("Orbital worker family",
                                             BackgroundPackages.indenture,
                                             MorphTables.indenture)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Orbital elite", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (2 to 2) -> UnresolvedBackground("A new star born above the Earth",
                                           BackgroundPackages.hypereliteMedia,
                                           MorphTables.sylph),
          (3 to 5) -> UnresolvedBackground("Orbital colonist; floating above the masses",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (6 to 12) -> SubTableBackground("Orbital colony staff", colonialStaffTable),
          (13 to 18) -> UnresolvedBackground("One of the lucky few to live above",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.lucky),
          (19 to 20) -> UnresolvedBackground("Orbital worker family",
                                             BackgroundPackages.indenture,
                                             MorphTables.indenture)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 5) -> UnresolvedBackground("Orbital elite", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (6 to 6) -> UnresolvedBackground("A new star born above the Earth",
                                           BackgroundPackages.hypereliteMedia,
                                           MorphTables.sylph),
          (7 to 10) -> UnresolvedBackground("Orbital colonist; floating above the masses",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (11 to 16) -> SubTableBackground("Orbital colony staff", colonialStaffTable),
          (17 to 19) -> UnresolvedBackground("One of the lucky few to live above",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.lucky),
          (20 to 20) -> UnresolvedBackground("Orbital worker family",
                                             BackgroundPackages.indenture,
                                             MorphTables.indenture)
        );
    }
  }

  private lazy val data4: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 10) -> SubTableBackground("Lunar elite", lunarEliteTable),
          (11 to 13) -> UnresolvedBackground("Lunar colonist: priviliged homesteader",
                                             BackgroundPackages.fallEvacueeEnclaver,
                                             MorphTables.splicer),
          (14 to 16) -> SubTableBackground("Lunar colony staff", colonialStaffTable),
          (17 to 19) -> UnresolvedBackground("Raised with a view of Earth",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (20 to 20) -> UnresolvedBackground("Lunar work force", BackgroundPackages.indenture, MorphTables.flat)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 5) -> SubTableBackground("Lunar elite", lunarEliteTable),
          (6 to 10) -> UnresolvedBackground("Lunar colonist: priviliged homesteader",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (11 to 16) -> SubTableBackground("Lunar colony staff", colonialStaffTable),
          (17 to 19) -> UnresolvedBackground("Raised with a view of Earth",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (20 to 20) -> UnresolvedBackground("Lunar work force", BackgroundPackages.indenture, MorphTables.flat)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 2) -> SubTableBackground("Lunar elite", lunarEliteTable),
          (3 to 8) -> UnresolvedBackground("Lunar colonist: priviliged homesteader",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (9 to 16) -> SubTableBackground("Lunar colony staff", colonialStaffTable),
          (17 to 18) -> UnresolvedBackground("Raised with a view of Earth",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (19 to 20) -> UnresolvedBackground("Lunar work force", BackgroundPackages.indenture, MorphTables.flat)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 5) -> SubTableBackground("Lunar elite", lunarEliteTable),
          (6 to 10) -> UnresolvedBackground("Lunar colonist: priviliged homesteader",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (11 to 17) -> SubTableBackground("Lunar colony staff", colonialStaffTable),
          (18 to 19) -> UnresolvedBackground("Raised with a view of Earth",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (20 to 20) -> UnresolvedBackground("Lunar work force", BackgroundPackages.indenture, MorphTables.flat)
        );
    }
  }

  private lazy val data5: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 10) -> UnresolvedBackground("Martian elite", BackgroundPackages.hypereliteScion, MorphsOR.olympian),
          (11 to 14) -> UnresolvedBackground("Martian colonist: privileged homesteader",
                                             BackgroundPackages.fallEvacueeEnclaver,
                                             MorphTables.splicer),
          (15 to 19) -> SubTableBackground("Martian colony staff", colonialStaffTable),
          (20 to 20) -> UnresolvedBackground("Risk-taking Martian settler",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Martian elite", BackgroundPackages.hypereliteScion, MorphsOR.olympian),
          (2 to 4) -> UnresolvedBackground("Martian colonist: privileged homesteader",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (5 to 10) -> SubTableBackground("Martian colony staff", colonialStaffTable),
          (11 to 16) -> UnresolvedBackground("Risk-taking Martian settler",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (17 to 17) -> UnresolvedBackground("Martian slave labor", BackgroundPackages.indenture, MorphTables.flat),
          (18 to 20) -> UnresolvedBackground("Pre-Barsoomian Martian nomad",
                                             BackgroundPackages.drifter,
                                             MorphTables.splicerOrFlat)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Martian elite", BackgroundPackages.hypereliteScion, MorphsOR.olympian),
          (2 to 10) -> UnresolvedBackground("Martian colonist: privileged homesteader",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (11 to 20) -> SubTableBackground("Martian colony staff", colonialStaffTable)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 4) -> UnresolvedBackground("Martian elite", BackgroundPackages.hypereliteScion, MorphsOR.olympian),
          (5 to 9) -> UnresolvedBackground("Martian colonist: privileged homesteader",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (10 to 17) -> SubTableBackground("Martian colony staff", colonialStaffTable),
          (18 to 19) -> UnresolvedBackground("Risk-taking Martian settler",
                                             BackgroundPackages.reinstantiatedCivilian,
                                             MorphTables.splicerOrFlat),
          (20 to 20) -> UnresolvedBackground("Pre-Barsoomian Martian nomad",
                                             BackgroundPackages.drifter,
                                             MorphTables.splicerOrFlat)
        );
    }
  }

  private lazy val data6: RollTable[BackgroundEntry] = {

    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 10) -> UnresolvedBackground("Pioneer dynasty", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (11 to 15) -> UnresolvedBackground("Venusian colonist: privileged homesteader",
                                             BackgroundPackages.fallEvacueeEnclaver,
                                             MorphTables.splicer),
          (16 to 20) -> SubTableBackground("Venusian colony staff", colonialStaffTable)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 2) -> UnresolvedBackground("Pioneer dynasty", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (3 to 6) -> UnresolvedBackground("Venusian colonist: privileged homesteader",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (7 to 17) -> SubTableBackground("Venusian colony staff", colonialStaffTable),
          (18 to 20) -> UnresolvedBackground("Mercurian slave labor",
                                             BackgroundPackages.indenture,
                                             MorphTables.flatOrCase)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Pioneer dynasty", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (2 to 6) -> UnresolvedBackground("Venusian colonist: privileged homesteader",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphTables.splicer),
          (7 to 20) -> SubTableBackground("Venusian colony staff", colonialStaffTable)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 6) -> UnresolvedBackground("Pioneer dynasty", BackgroundPackages.hypereliteScion, MorphTables.exalt),
          (7 to 12) -> UnresolvedBackground("Venusian colonist: privileged homesteader",
                                            BackgroundPackages.fallEvacueeEnclaver,
                                            MorphTables.splicer),
          (13 to 20) -> SubTableBackground("Venusian colony staff", colonialStaffTable)
        );
    }
  }

  private lazy val data7: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 5) -> UnresolvedBackground("Extropia founders",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphsAC.bouncer),
          (6 to 6) -> UnresolvedBackground("Jovian colonist: shelter among giants",
                                           BackgroundPackages.isolateSurvivalist,
                                           MorphTables.jovian),
          (7 to 14) -> UnresolvedBackground("Titanian colonist",
                                            BackgroundPackages.colonistScienceStaff,
                                            MorphTables.titanian),
          (15 to 19) -> UnresolvedBackground("Anarchist colonist",
                                             BackgroundPackages.colonistTechStaff,
                                             MorphTables.anarchist),
          (20 to 20) -> SubTableBackground("Small colony outpost", colonialStaffTable)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 4) -> UnresolvedBackground("Extropia founders",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphsAC.bouncer),
          (5 to 6) -> UnresolvedBackground("Jovian colonist: shelter among giants",
                                           BackgroundPackages.isolateSurvivalist,
                                           MorphTables.jovian),
          (7 to 10) -> UnresolvedBackground("Titanian colonist",
                                            BackgroundPackages.colonistScienceStaff,
                                            MorphTables.titanian),
          (11 to 15) -> UnresolvedBackground("Anarchist colonist",
                                             BackgroundPackages.colonistTechStaff,
                                             MorphTables.anarchist),
          (16 to 19) -> SubTableBackground("Small colony outpost", colonialStaffTable),
          (20 to 20) -> UnresolvedBackground("Asteroid miner", BackgroundPackages.indenture, MorphTables.miner)
        );
      case Archetype.Fighter | Archetype.Scientist =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Extropia founders",
                                           BackgroundPackages.fallEvacueeEnclaver,
                                           MorphsAC.bouncer),
          (2 to 2) -> UnresolvedBackground("Jovian colonist: shelter among giants",
                                           BackgroundPackages.isolateSurvivalist,
                                           MorphTables.jovian),
          (3 to 3) -> UnresolvedBackground("Titanian colonist",
                                           BackgroundPackages.colonistScienceStaff,
                                           MorphTables.titanian),
          (4 to 5) -> UnresolvedBackground("Anarchist colonist",
                                           BackgroundPackages.colonistTechStaff,
                                           MorphTables.anarchist),
          (6 to 8) -> SubTableBackground("Small colony outpost", colonialStaffTable),
          (9 to 10) -> UnresolvedBackground("Asteroid miner", BackgroundPackages.indenture, MorphTables.miner)
        );
    }
  }

  private lazy val data8: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Wandering the system", BackgroundPackages.drifter, MorphTables.anarchist),
          (2 to 8) -> UnresolvedBackground("Found freedom in orbit", BackgroundPackages.originalScum, MorphTables.scum),
          (9 to 10) -> UnresolvedBackground("Supply ship crew",
                                            BackgroundPackages.colonistFlightStaff,
                                            MorphTables.supply)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 3) -> UnresolvedBackground("Wandering the system", BackgroundPackages.drifter, MorphTables.anarchist),
          (4 to 7) -> UnresolvedBackground("Found freedom in orbit", BackgroundPackages.originalScum, MorphTables.scum),
          (8 to 8) -> UnresolvedBackground("Supply ship crew",
                                           BackgroundPackages.colonistFlightStaff,
                                           MorphTables.supply),
          (9 to 10) -> UnresolvedBackground("Migrant worker", BackgroundPackages.indenture, MorphTables.miner)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 2) -> UnresolvedBackground("Wandering the system", BackgroundPackages.drifter, MorphTables.anarchist),
          (3 to 7) -> UnresolvedBackground("Found freedom in orbit", BackgroundPackages.originalScum, MorphTables.scum),
          (8 to 9) -> UnresolvedBackground("Supply ship crew",
                                           BackgroundPackages.colonistFlightStaff,
                                           MorphTables.supply),
          (10 to 10) -> UnresolvedBackground("Migrant worker", BackgroundPackages.indenture, MorphTables.miner)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 3) -> UnresolvedBackground("Wandering the system", BackgroundPackages.drifter, MorphTables.anarchist),
          (4 to 9) -> UnresolvedBackground("Found freedom in orbit", BackgroundPackages.originalScum, MorphTables.scum),
          (10 to 10) -> UnresolvedBackground("Supply ship crew",
                                             BackgroundPackages.colonistFlightStaff,
                                             MorphTables.supply)
        );
    }
  }

  private lazy val data9: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 1) -> SubTableBackground("Almost human", almostHumanTable),
          (2 to 3) -> SubTableBackground("An experiment gone horribly wrong", lostTable),
          (4 to 7) -> UnresolvedBackground("Second-class citizenship was not for you",
                                           BackgroundPackages.upliftEscapee,
                                           MorphTables.uplifts),
          (8 to 10) -> UnresolvedBackground("Living proof that uplift works",
                                            BackgroundPackages.upliftStandard,
                                            MorphTables.uplifts)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 2) -> SubTableBackground("Almost human", almostHumanTable),
          (3 to 4) -> SubTableBackground("More machine than man", machineTable),
          (5 to 6) -> SubTableBackground("An experiment gone horribly wrong", lostTable),
          (7 to 8) -> UnresolvedBackground("Second-class citizenship was not for you",
                                           BackgroundPackages.upliftEscapee,
                                           MorphTables.uplifts),
          (9 to 10) -> UnresolvedBackground("Living proof that uplift works",
                                            BackgroundPackages.upliftStandard,
                                            MorphTables.uplifts)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 2) -> SubTableBackground("Almost human", almostHumanTable),
          (3 to 5) -> SubTableBackground("More machine than man", machineTable),
          (6 to 8) -> SubTableBackground("Created by and for science", scienceTable),
          (9 to 9) -> UnresolvedBackground("Second-class citizenship was not for you",
                                           BackgroundPackages.upliftEscapee,
                                           MorphTables.uplifts),
          (10 to 10) -> UnresolvedBackground("Living proof that uplift works",
                                             BackgroundPackages.upliftStandard,
                                             MorphTables.uplifts)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 1) -> SubTableBackground("Almost human", almostHumanTable),
          (2 to 3) -> SubTableBackground("More machine than man", machineTable),
          (4 to 7) -> SubTableBackground("Created by and for science", scienceTable),
          (8 to 8) -> SubTableBackground("An experiment gone horribly wrong", lostTable),
          (9 to 9) -> UnresolvedBackground("Second-class citizenship was not for you",
                                           BackgroundPackages.upliftEscapee,
                                           MorphTables.uplifts),
          (10 to 10) -> UnresolvedBackground("Living proof that uplift works",
                                             BackgroundPackages.upliftStandard,
                                             MorphTables.uplifts)
        );
    }
  }
  private lazy val colonialStaffTable: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 8) -> UnresolvedBackground("Born to lead",
                                           BackgroundPackages.colonistCommandStaff,
                                           MorphTables.colonial1),
          (9 to 9) -> UnresolvedBackground("Space crew", BackgroundPackages.colonistFlightStaff, MorphTables.colonial2),
          (10 to 10) -> UnresolvedBackground("Researcher",
                                             BackgroundPackages.colonistScienceStaff,
                                             MorphTables.colonial4)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Born to lead",
                                           BackgroundPackages.colonistCommandStaff,
                                           MorphTables.colonial1),
          (2 to 2) -> UnresolvedBackground("Space crew", BackgroundPackages.colonistFlightStaff, MorphTables.colonial2),
          (3 to 10) -> UnresolvedBackground("Peacekeeper",
                                            BackgroundPackages.colonistSecurityStaff,
                                            MorphTables.colonial3)
        );
      case Archetype.Hacker =>
        RollTable(
          (1 to 2) -> UnresolvedBackground("Researcher",
                                           BackgroundPackages.colonistScienceStaff,
                                           MorphTables.colonial4),
          (3 to 10) -> UnresolvedBackground("You kept the habitat functioning",
                                            BackgroundPackages.colonistTechStaff,
                                            MorphTables.colonial5)
        );
      case Archetype.Scientist =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Born to lead",
                                           BackgroundPackages.colonistCommandStaff,
                                           MorphTables.colonial1),
          (2 to 2) -> UnresolvedBackground("Space crew", BackgroundPackages.colonistFlightStaff, MorphTables.colonial2),
          (3 to 9) -> UnresolvedBackground("Researcher",
                                           BackgroundPackages.colonistScienceStaff,
                                           MorphTables.colonial4),
          (10 to 10) -> UnresolvedBackground("You kept the habitat functioning",
                                             BackgroundPackages.colonistTechStaff,
                                             MorphTables.colonial5)
        );
    }
  }

  private lazy val lunarEliteTable: RollTable[BackgroundEntry] = RollTable(
    (1 to 7) -> UnresolvedBackground("Scion", BackgroundPackages.hypereliteScion, MorphTables.exalt),
    (8 to 10) -> UnresolvedBackground("Celebrity", BackgroundPackages.hypereliteMedia, MorphTables.sylph)
  );

  private lazy val almostHumanTable: RollTable[BackgroundEntry] = RollTable(
    (1 to 3) -> UnresolvedBackground("Post-Fall AGI", BackgroundPackages.infolifeHumanities, MorphTables.infomorph),
    (4 to 10) -> UnresolvedBackground("Pre-Fall AGI", BackgroundPackages.infolifeHumanities, MorphTables.infomorph)
  );

  private lazy val machineTable: RollTable[BackgroundEntry] = RollTable(
    (1 to 3) -> UnresolvedBackground("Post-Fall AGI", BackgroundPackages.infolifeMachine, MorphTables.infomorph),
    (4 to 10) -> UnresolvedBackground("Pre-Fall AGI", BackgroundPackages.infolifeMachine, MorphTables.infomorph)
  );

  private lazy val scienceTable: RollTable[BackgroundEntry] = RollTable(
    (1 to 3) -> UnresolvedBackground("Post-Fall AGI", BackgroundPackages.infolifeResearch, MorphTables.infomorph),
    (4 to 10) -> UnresolvedBackground("Pre-Fall AGI", BackgroundPackages.infolifeResearch, MorphTables.infomorph)
  );

  private lazy val lostTable: RollTable[BackgroundEntry] = {
    archetype match {
      case Archetype.Butterfly =>
        RollTable(
          (1 to 1) -> UnresolvedBackground("Disturbed Child (Lost Generation)",
                                           BackgroundPackages.lostDisturbed,
                                           MorphTables.futura),
          (2 to 10) -> UnresolvedBackground("Masked Normalcy (Lost Generation)",
                                            BackgroundPackages.lostMasked,
                                            MorphTables.futura)
        );
      case Archetype.Fighter =>
        RollTable(
          (1 to 7) -> UnresolvedBackground("Disturbed Child (Lost Generation)",
                                           BackgroundPackages.lostDisturbed,
                                           MorphTables.futura),
          (8 to 10) -> UnresolvedBackground("Masked Normalcy (Lost Generation)",
                                            BackgroundPackages.lostMasked,
                                            MorphTables.futura)
        );
      case Archetype.Hacker | Archetype.Scientist =>
        RollTable(
          (1 to 5) -> UnresolvedBackground("Disturbed Child (Lost Generation)",
                                           BackgroundPackages.lostDisturbed,
                                           MorphTables.futura),
          (6 to 10) -> UnresolvedBackground("Masked Normalcy (Lost Generation)",
                                            BackgroundPackages.lostMasked,
                                            MorphTables.futura)
        );

    }
  }

  override def label: String = "Archetype Background";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    val background = origin.map(BackgroundClasses.fromOrigin).getOrElse(data1.randomElement(rand).get);
    val bg = getBackground(rand, background.nextTable);
    val resolved = bg.resolve(rand, background.label);
    resolved
  }
  private def getBackground(rand: Random, table: Int): BackgroundEntry = table match {
    case 2 => data2.randomElement(rand).get
    case 3 => data3.randomElement(rand).get
    case 4 => data4.randomElement(rand).get
    case 5 => data5.randomElement(rand).get
    case 6 => data6.randomElement(rand).get
    case 7 => data7.randomElement(rand).get
    case 8 => data8.randomElement(rand).get
    case 9 => data9.randomElement(rand).get
    case _ => ???
  }

  private object MorphTables {
    val infomorph = RollConstant(MorphsGL.infomorph);
    val exalt = RollConstant(MorphsDF.exalt);
    val flat = RollConstant(MorphsDF.flat);
    val flatOrCase = RollTable((1 to 7) -> MorphsDF.flat, (8 to 10) -> MorphsAC.`case`);
    val futura = RollConstant(MorphsDF.futura);
    val sylph = RollConstant(MorphsS.sylph);
    val splicer = RollConstant(MorphsS.splicer);
    val splicerOrFlat = RollTable((1 to 4) -> MorphsS.splicer, (5 to 10) -> MorphsDF.flat);
    val lucky = RollTable((1 to 4) -> MorphsS.splicer, (5 to 9) -> MorphsDF.flat, (10 to 10) -> MorphsAC.bouncer);
    val indenture = RollTable((1 to 8) -> MorphsDF.flat, (9 to 10) -> MorphsAC.bouncer);
    val colonial1 = RollTable((1 to 7) -> MorphsS.splicer, (8 to 10) -> MorphsDF.exalt);
    val colonial2 = RollTable((1 to 5) -> MorphsS.splicer, (6 to 10) -> MorphsAC.bouncer);
    val colonial3 = RollTable((1 to 3) -> MorphsDF.flat, (4 to 7) -> MorphsS.splicer, (8 to 10) -> MorphsOR.olympian);
    val colonial4 = RollTable((1 to 6) -> MorphsS.splicer, (7 to 10) -> MorphsMN.menton);
    val colonial5 = RollTable((1 to 3) -> MorphsDF.flat, (4 to 8) -> MorphsS.splicer, (9 to 10) -> MorphsAC.bouncer);
    val jovian = RollTable((1 to 5) -> MorphsDF.flat, (6 to 8) -> MorphsS.splicer, (9 to 10) -> MorphsOR.olympian);
    val titanian = RollTable((1 to 8) -> MorphsS.splicer, (9 to 10) -> MorphsMN.menton);
    val anarchist = RollTable((1 to 6) -> MorphsS.splicer, (7 to 10) -> MorphsAC.bouncer);
    val miner = RollTable((1 to 4) -> MorphsDF.flat, (5 to 7) -> MorphsAC.`case`, (8 to 10) -> MorphsAC.bouncer);
    val scum: RollTableLike[MorphModel] = RollSubTables(
      RollTable((1 to 5) -> MorphsS.splicer,
                (6 to 8) -> MorphsAC.bouncer,
                (9 to 10) -> creationpackages.ChoosingAMorph.data)
    );
    val supply = RollTable((1 to 4) -> MorphsS.splicer, (5 to 7) -> MorphsAC.bouncer, (8 to 10) -> MorphsGL.hibernoid);
    val uplifts: RollTable[MorphModel] = {
      archetype match {
        case Archetype.Butterfly =>
          RollTable(
            (1 to 60) -> MorphsMN.neoAvian,
            (61 to 66) -> MorphsMN.neoHominid,
            (67 to 68) -> MorphsOR.octomorph,
            (69 to 69) -> MorphsMN.neoNeanderthal,
            (70 to 79) -> MorphsMN.neoBeluga,
            (80 to 88) -> MorphsMN.neoDolphin,
            (89 to 89) -> MorphsMN.neoGorilla,
            (90 to 97) -> MorphsMN.neoOrca,
            (98 to 98) -> MorphsMN.neoPig,
            (99 to 99) -> MorphsMN.neoPorpoise,
            (100 to 100) -> MorphsMN.neoWhale
          );
        case Archetype.Fighter =>
          RollTable(
            (1 to 20) -> MorphsMN.neoAvian,
            (21 to 50) -> MorphsMN.neoHominid,
            (51 to 60) -> MorphsOR.octomorph,
            (61 to 65) -> MorphsMN.neoNeanderthal,
            (66 to 66) -> MorphsMN.neoBeluga,
            (67 to 68) -> MorphsMN.neoDolphin,
            (69 to 79) -> MorphsMN.neoGorilla,
            (80 to 90) -> MorphsMN.neoOrca,
            (91 to 98) -> MorphsMN.neoPig,
            (99 to 99) -> MorphsMN.neoPorpoise,
            (100 to 100) -> MorphsMN.neoWhale
          );
        case Archetype.Hacker =>
          RollTable(
            (1 to 40) -> MorphsMN.neoAvian,
            (41 to 60) -> MorphsMN.neoHominid,
            (61 to 74) -> MorphsOR.octomorph,
            (75 to 75) -> MorphsMN.neoNeanderthal,
            (76 to 76) -> MorphsMN.neoBeluga,
            (77 to 80) -> MorphsMN.neoDolphin,
            (81 to 92) -> MorphsMN.neoGorilla,
            (93 to 93) -> MorphsMN.neoOrca,
            (94 to 94) -> MorphsMN.neoPig,
            (95 to 99) -> MorphsMN.neoPorpoise,
            (100 to 100) -> MorphsMN.neoWhale
          );
        case Archetype.Scientist =>
          RollTable(
            (1 to 40) -> MorphsMN.neoAvian,
            (41 to 60) -> MorphsMN.neoHominid,
            (61 to 78) -> MorphsOR.octomorph,
            (79 to 79) -> MorphsMN.neoBeluga,
            (80 to 85) -> MorphsMN.neoDolphin,
            (86 to 92) -> MorphsMN.neoGorilla,
            (93 to 93) -> MorphsMN.neoOrca,
            (94 to 94) -> MorphsMN.neoPig,
            (95 to 99) -> MorphsMN.neoPorpoise,
            (100 to 100) -> MorphsMN.neoWhale
          );
      }
    }
  }
}

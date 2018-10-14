package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.compendium.MorphModel
import com.lkroll.ep.chargen.creationpackages.PackageLevel
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.data._

case class YouthPath(youthType: String, childhoods: List[ConcreteChildhood])
case class ConcreteChildhood(
  label:             String,
  group:             String,
  pkg:               BackgroundPackage,
  startingMorph:     MorphModel,
  nextPath:          AdultPathIndex,
  skipPreFallReason: Option[String]    = None,
  mustComeFirst:     Boolean           = false)

object YouthPathTable extends Table {
  import Implicits.constToRollTable;

  override type Result = YouthPath;

  case class YouthType(label: String, backgroundPackages: List[PackageLevel])
  case class Background(label: String, nextTable: Int)
  sealed trait ChildhoodEntry {
    def resolve(rand: Random, backgroundGroup: String, backgroundLevel: PackageLevel): ConcreteChildhood;
  }
  case class Childhood(
    label:             String,
    pkg:               PackageGroup[BackgroundPackage],
    startingMorph:     RollTableLike[MorphModel],
    nextPath:          RollTableLike[AdultPathIndex],
    skipPreFallReason: Option[String]                  = None,
    mustComeFirst:     Boolean                         = false) extends ChildhoodEntry {
    override def resolve(rand: Random, backgroundGroup: String, backgroundLevel: PackageLevel): ConcreteChildhood = {
      ConcreteChildhood(
        label,
        backgroundGroup,
        pkg.randomElement(rand).flatMap(_.ofLevel(backgroundLevel)).get,
        startingMorph.randomElement(rand).get,
        nextPath.randomElement(rand).get,
        skipPreFallReason,
        mustComeFirst)
    }
  }
  case class SubTableChildhood(label: String, table: RollTable[ChildhoodEntry]) extends ChildhoodEntry {
    override def resolve(rand: Random, backgroundGroup: String, backgroundLevel: PackageLevel): ConcreteChildhood = {
      val childhood = table.randomElement(rand).get;
      val c = childhood.resolve(rand, backgroundGroup, backgroundLevel);
      c.copy(label = s"${label} - ${c.label}")
    }
  }

  //private val subtables = Array(data0, data1);

  private val data0: RollTable[YouthType] = RollTable(
    (1 to 6) -> YouthType("Wholesome Youth", List(PackageLevel.Influential)),
    (7 to 9) -> YouthType("Split Youth", List(PackageLevel.Basic, PackageLevel.Basic)),
    (10 to 10) -> YouthType("Fractured Youth", List(PackageLevel.Basic, PackageLevel.Basic, PackageLevel.Basic)));

  private val data1: RollTable[Background] = RollTable(
    (1 to 50) -> Background("Earthborn", 2),
    (51 to 60) -> Background("Orbital", 3),
    (61 to 68) -> Background("Lunar", 4),
    (69 to 76) -> Background("Martian Settler", 5),
    (77 to 82) -> Background("Sunward Hab", 6),
    (83 to 89) -> Background("Rimward Hab", 7),
    (90 to 95) -> Background("Migrant", 8),
    (96 to 100) -> Background("Created not Born", 9));

  private lazy val data2: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Born with a silver nanoswarm in your blood", BackgroundPackages.hypereliteScion, MorphTables.exalt, NextPathTables.elite),
    (2 to 2) -> Childhood("Celebrity child", BackgroundPackages.hypereliteMedia, MorphTables.sylph, NextPathTables.elite),
    (3 to 5) -> Childhood("Privileged: enclave born", BackgroundPackages.fallEvacueeEnclaver, MorphTables.splicer, NextPathTables.enclaver),
    (6 to 7) -> Childhood("Precariat: poverty just a step away", BackgroundPackages.reinstantiatedCivilian, MorphTables.splicerOrFlat, NextPathTables.civilian),
    (8 to 8) -> Childhood("Troubled: raised among disaster or war", BackgroundPackages.fallEvacueeUnderclass, MorphTables.flat, NextPathTables.cca),
    (9 to 9) -> Childhood("Raised on the street", BackgroundPackages.streetRat, MorphTables.flat, NextPathTables.criminal),
    (10 to 10) -> Childhood("Raised in a collective/commune grouping", BackgroundPackages.reinstantiatedCivilian, MorphTables.splicerOrFlat, NextPathTables.autonomist));

  private lazy val data3: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Orbital elite", BackgroundPackages.hypereliteScion, MorphTables.exalt, NextPathTables.elite),
    (2 to 2) -> Childhood("A new star born above the Earth", BackgroundPackages.hypereliteMedia, MorphTables.sylph, NextPathTables.elite),
    (3 to 4) -> Childhood("Orbital colonist; floating above the masses", BackgroundPackages.fallEvacueeEnclaver, MorphTables.splicer, NextPathTables.enclaver),
    (5 to 6) -> SubTableChildhood("Orbital colony staff", colonialStaffTable),
    (7 to 8) -> Childhood("One of the lucky few to live above", BackgroundPackages.reinstantiatedCivilian, MorphTables.lucky, NextPathTables.civilian),
    (9 to 10) -> Childhood("Orbital worker family", BackgroundPackages.indenture, MorphTables.indenture, NextPathTables.indenture));

  private lazy val data4: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> SubTableChildhood("Lunar elite", lunarEliteTable),
    (2 to 3) -> Childhood("Lunar colonist: priviliged homesteader", BackgroundPackages.fallEvacueeEnclaver, MorphTables.splicer, NextPathTables.enclaver),
    (4 to 5) -> SubTableChildhood("Lunar colony staff", colonialStaffTable),
    (6 to 7) -> Childhood("Raised with a view of Earth", BackgroundPackages.reinstantiatedCivilian, MorphTables.splicerOrFlat, NextPathTables.civilian),
    (8 to 10) -> Childhood("Lunar work force", BackgroundPackages.indenture, MorphTables.flat, NextPathTables.indenture));

  private lazy val data5: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Martian elite", BackgroundPackages.hypereliteScion, MorphsOR.olympian, NextPathTables.elite),
    (2 to 3) -> Childhood("Martian colonist: privileged homesteader", BackgroundPackages.fallEvacueeEnclaver, MorphTables.splicer, NextPathTables.enclaver),
    (4 to 5) -> SubTableChildhood("Martian colony staff", colonialStaffTable),
    (6 to 6) -> Childhood("Risk-taking Martian settler", BackgroundPackages.reinstantiatedCivilian, MorphTables.splicerOrFlat, NextPathTables.civilian),
    (7 to 9) -> Childhood("Martian slave labor", BackgroundPackages.indenture, MorphTables.flat, NextPathTables.indenture),
    (10 to 10) -> Childhood("Pre-Barsoomian Martian nomad", BackgroundPackages.drifter, MorphTables.splicerOrFlat, NextPathTables.autonomist));

  private lazy val data6: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Pioneer dynasty", BackgroundPackages.hypereliteScion, MorphTables.exalt, NextPathTables.elite),
    (2 to 3) -> Childhood("Venusian colonist: privileged homesteader", BackgroundPackages.fallEvacueeEnclaver, MorphTables.splicer, NextPathTables.enclaver),
    (4 to 6) -> SubTableChildhood("Venusian colony staff", colonialStaffTable),
    (7 to 10) -> Childhood("Mercurian slave labor", BackgroundPackages.indenture, MorphTables.flatOrCase, NextPathTables.indenture));

  private lazy val data7: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Extropia founders", BackgroundPackages.fallEvacueeEnclaver, MorphsAC.bouncer, NextPathTables.elite),
    (2 to 2) -> Childhood("Jovian colonist: shelter among giants", BackgroundPackages.isolateSurvivalist, MorphTables.jovian, NextPathTables.military),
    (3 to 3) -> Childhood("Titanian colonist", BackgroundPackages.colonistScienceStaff, MorphTables.titanian, NextPathTables.scientist),
    (4 to 5) -> Childhood("Anarchist colonist", BackgroundPackages.colonistTechStaff, MorphTables.anarchist, NextPathTables.autonomist),
    (6 to 8) -> SubTableChildhood("Small colony outpost", colonialStaffTable),
    (9 to 10) -> Childhood("Asteroid miner", BackgroundPackages.indenture, MorphTables.miner, NextPathTables.indenture));

  private lazy val data8: RollTable[ChildhoodEntry] = RollTable(
    (1 to 3) -> Childhood("Wandering the system", BackgroundPackages.drifter, MorphTables.anarchist, NextPathTables.spacer),
    (4 to 5) -> Childhood("Found freedom in orbit", BackgroundPackages.originalScum, MorphTables.scum, NextPathTables.autonomist),
    (6 to 8) -> Childhood("Supply ship crew", BackgroundPackages.colonistFlightStaff, MorphTables.supply, NextPathTables.spacer),
    (9 to 10) -> Childhood("Migrant worker", BackgroundPackages.indenture, MorphTables.miner, NextPathTables.indenture));

  private lazy val data9: RollTable[ChildhoodEntry] = RollTable(
    (1 to 3) -> SubTableChildhood("Almost human", almostHumanTable),
    (4 to 5) -> SubTableChildhood("More machine than man", machineTable),
    (6 to 7) -> SubTableChildhood("Created by and for science", scienceTable),
    (8 to 8) -> SubTableChildhood("An experiment gone horribly wrong", lostTable),
    (9 to 9) -> Childhood("Second-class citizenship was not for you", BackgroundPackages.upliftEscapee, ChoosingAMorph.uplifts, NextPathTables.civilian),
    (10 to 10) -> Childhood("Living proof that uplift works", BackgroundPackages.upliftStandard, ChoosingAMorph.uplifts, NextPathTables.civilian));

  private lazy val colonialStaffTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 1) -> Childhood("Born to lead", BackgroundPackages.colonistCommandStaff, MorphTables.colonial1, NextPathTables.enclaver),
    (2 to 3) -> Childhood("Space crew", BackgroundPackages.colonistFlightStaff, MorphTables.colonial2, NextPathTables.spacer),
    (4 to 5) -> Childhood("Peacekeeper", BackgroundPackages.colonistSecurityStaff, MorphTables.colonial3, NextPathTables.military),
    (6 to 7) -> Childhood("Researcher", BackgroundPackages.colonistScienceStaff, MorphTables.colonial4, NextPathTables.scientist),
    (8 to 10) -> Childhood("You kept the habitat functioning", BackgroundPackages.colonistTechStaff, MorphTables.colonial5, NextPathTables.techie));

  private lazy val lunarEliteTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 7) -> Childhood("Scion", BackgroundPackages.hypereliteScion, MorphTables.exalt, NextPathTables.elite),
    (8 to 10) -> Childhood("Celebrity", BackgroundPackages.hypereliteMedia, MorphTables.sylph, NextPathTables.elite));

  private lazy val almostHumanTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 3) -> Childhood("Post-Fall AGI", BackgroundPackages.infolifeHumanities, MorphTables.infomorph, NextPathTables.civilian, skipPreFallReason = Some("Post-Fall AGI"), mustComeFirst = true),
    (4 to 10) -> Childhood("Pre-Fall AGI", BackgroundPackages.infolifeHumanities, MorphTables.infomorph, NextPathTables.civilian, mustComeFirst = true));

  private lazy val machineTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 3) -> Childhood("Post-Fall AGI", BackgroundPackages.infolifeMachine, MorphTables.infomorph, NextPathTables.techie, skipPreFallReason = Some("Post-Fall AGI"), mustComeFirst = true),
    (4 to 10) -> Childhood("Pre-Fall AGI", BackgroundPackages.infolifeMachine, MorphTables.infomorph, NextPathTables.techie, mustComeFirst = true));

  private lazy val scienceTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 3) -> Childhood("Post-Fall AGI", BackgroundPackages.infolifeResearch, MorphTables.infomorph, NextPathTables.scientist, skipPreFallReason = Some("Post-Fall AGI"), mustComeFirst = true),
    (4 to 10) -> Childhood("Pre-Fall AGI", BackgroundPackages.infolifeResearch, MorphTables.infomorph, NextPathTables.scientist, mustComeFirst = true));

  private lazy val lostTable: RollTable[ChildhoodEntry] = RollTable(
    (1 to 5) -> Childhood("Disturbed Child (Lost Generation)", BackgroundPackages.lostDisturbed, MorphTables.futura, NextPathTables.newPath, skipPreFallReason = Some("Lost"), mustComeFirst = true),
    (6 to 10) -> Childhood("Masked Normalcy (Lost Generation)", BackgroundPackages.lostMasked, MorphTables.futura, NextPathTables.newPath, skipPreFallReason = Some("Lost"), mustComeFirst = true));

  override def label: String = "Youth Path";
  override def source: String = "Transhuman p.56-58";
  override def roll(rand: Random): Result = {
    val youthType = data0.randomElement(rand).get;
    var backgrounds = Set.empty[String];
    var childhoods: List[ConcreteChildhood] = Nil;
    youthType.backgroundPackages.foreach { lvl =>
      var retry = true;
      while (retry) {
        val background = data1.randomElement(rand).get;
        val childhood = getChildhood(rand, background.nextTable);
        val resolved = childhood.resolve(rand, background.label, lvl);
        if (!backgrounds.contains(resolved.pkg.label)) {
          if (resolved.mustComeFirst) {
            childhoods match {
              case head :: rest if head.mustComeFirst => () // can't have two first entries -> retry
              case rest => {
                backgrounds += resolved.pkg.label;
                childhoods = resolved :: rest;
                retry = false;
              }
            }
          } else {
            backgrounds += resolved.pkg.label;
            childhoods = childhoods match {
              case head :: rest if head.mustComeFirst => head :: resolved :: rest
              case rest                               => resolved :: rest
            }
            retry = false;
          }
        }
      }
    }
    childhoods.find(c => c.skipPreFallReason.isDefined) match {
      case Some(c) => childhoods = List(c) // ignore all other backgrounds (upgrade background package to 3PP?)
      case None    => // no change
    }
    YouthPath(youthType.label, childhoods)
  }

  private def getChildhood(rand: Random, table: Int): ChildhoodEntry = table match {
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
    val scum: RollTableLike[MorphModel] = RollSubTables(RollTable(
      (1 to 5) -> MorphsS.splicer,
      (6 to 8) -> MorphsAC.bouncer,
      (9 to 10) -> creationpackages.ChoosingAMorph.data));
    val supply = RollTable((1 to 4) -> MorphsS.splicer, (5 to 7) -> MorphsAC.bouncer, (8 to 10) -> MorphsGL.hibernoid);
  }
  private object NextPathTables {
    val newPath = RollConstant(AdultPathIndex(1));
    val elite = RollConstant(AdultPathIndex(5));
    val enclaver = RollConstant(AdultPathIndex(6));
    val civilian = RollConstant(AdultPathIndex(3));
    val cca = RollTable(
      (1 to 5) -> AdultPathIndex(4),
      (6 to 9) -> AdultPathIndex(3),
      (10 to 10) -> AdultPathIndex(2));
    val criminal = RollConstant(AdultPathIndex(4));
    val autonomist = RollConstant(AdultPathIndex(2));
    val indenture = RollConstant(AdultPathIndex(7));
    val spacer = RollConstant(AdultPathIndex(10));
    val military = RollConstant(AdultPathIndex(8));
    val scientist = RollConstant(AdultPathIndex(9));
    val techie = RollConstant(AdultPathIndex(11));
  }
}

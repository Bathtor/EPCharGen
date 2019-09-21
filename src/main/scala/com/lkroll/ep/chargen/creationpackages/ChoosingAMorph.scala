package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.compendium.{MorphModel, MorphType}
import com.lkroll.ep.chargen.Random
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.data._

sealed trait MorphFilter {
  def matches(morph: MorphModel): Boolean;
}
object MorphFilter {
  case class MType(tpe: MorphType) extends MorphFilter {
    def matches(morph: MorphModel): Boolean = {
      morph.morphType == tpe
    }
  }
  case object Uplift extends MorphFilter {
    lazy val uplifts = List(
      MorphsMN.neoAvian,
      MorphsMN.neoHominid,
      MorphsOR.octomorph,
      MorphsMN.neoNeanderthal,
      MorphsMN.neoBeluga,
      MorphsMN.neoDolphin,
      MorphsMN.neoGorilla,
      MorphsMN.neoOrca,
      MorphsMN.neoPig,
      MorphsMN.neoPorpoise,
      MorphsMN.neoWhale
    ).map(_.name);

    def matches(morph: MorphModel): Boolean = {
      uplifts.contains(morph.name)
    }
  }
}

object ChoosingAMorph {

  def randIgnore(rand: Random, filters: MorphFilter*): MorphModel = {
    var res: Option[MorphModel] = None;
    while (res.isEmpty) {
      res = data.randomElement(rand);
      val morph = res.get;
      if (filters.exists(_.matches(morph))) {
        res = None;
      }
    }
    res.get
  }

  def randOnly(rand: Random, filters: MorphFilter*): MorphModel = {
    var res: Option[MorphModel] = None;
    while (res.isEmpty) {
      res = data.randomElement(rand);
      val morph = res.get;
      if (!filters.exists(_.matches(morph))) {
        res = None;
      }
    }
    res.get
  }

  def randMorph(rand: Random): MorphModel = data.randomElement(rand).get;

  lazy val data: RollTableLike[MorphModel] = RollSubTables(
    RollTable((1 to 50) -> biomorphs,
              (51 to 55) -> uplifts,
              (56 to 65) -> pods,
              (66 to 95) -> synthmorphs,
              (96 to 100) -> infomorphs)
  );
  lazy val biomorphs: RollTable[MorphModel] = RollTable(
    (1 to 3) -> MorphsDF.flat,
    (4 to 13) -> MorphsS.splicer,
    (14 to 21) -> MorphsDF.exalt,
    (22 to 26) -> MorphsMN.menton,
    (27 to 34) -> MorphsOR.olympian,
    (35 to 39) -> MorphsS.sylph,
    (40 to 46) -> MorphsAC.bouncer,
    (47 to 49) -> MorphsDF.fury,
    (50 to 50) -> MorphsDF.futura,
    (51 to 53) -> MorphsGL.ghost,
    (54 to 56) -> MorphsGL.hibernoid,
    (57 to 59) -> MorphsMN.neotenic,
    (60 to 62) -> MorphsOR.remade,
    (63 to 69) -> MorphsOR.ruster,
    (70 to 70) -> MorphsGL.lunarFlyer,
    (71 to 72) -> MorphsMN.martianAlpiner,
    (73 to 73) -> MorphsS.salamander,
    (74 to 74) -> MorphsS.surya,
    (75 to 75) -> MorphsTX.venusianGlider,
    (76 to 77) -> MorphsGL.hazer,
    (78 to 78) -> MorphsGL.hulder,
    (79 to 79) -> MorphsGL.hyperbright,
    (80 to 80) -> MorphsOR.ringFlyer,
    (81 to 81) -> MorphsS.selkie,
    (82 to 82) -> MorphsAC.aquanaut,
    (83 to 85) -> MorphsAC.crasher,
    (86 to 86) -> MorphsDF.dvergr,
    (87 to 87) -> MorphsAC.ariel,
    (88 to 89) -> MorphsAC.bruiser,
    (90 to 90) -> MorphsAC.cloudSkate,
    (91 to 91) -> MorphsDF.faust,
    (92 to 92) -> MorphsDF.freeman,
    (93 to 93) -> MorphsGL.grey,
    (94 to 95) -> MorphsMN.nomad,
    (96 to 99) -> MorphsOR.observer,
    (100 to 100) -> MorphsTX.theseus
  );
  lazy val uplifts: RollTable[MorphModel] = RollTable(
    (1 to 30) -> MorphsMN.neoAvian,
    (31 to 50) -> MorphsMN.neoHominid,
    (51 to 70) -> MorphsOR.octomorph,
    (71 to 75) -> MorphsMN.neoNeanderthal,
    (76 to 76) -> MorphsMN.neoBeluga,
    (77 to 77) -> MorphsMN.neoDolphin,
    (78 to 92) -> MorphsMN.neoGorilla,
    (93 to 93) -> MorphsMN.neoOrca,
    (94 to 98) -> MorphsMN.neoPig,
    (99 to 99) -> MorphsMN.neoPorpoise,
    (100 to 100) -> MorphsMN.neoWhale
  );
  lazy val pods: RollTable[MorphModel] = RollTable(
    (1 to 15) -> MorphsOR.pleasurePod,
    (16 to 30) -> MorphsTX.workerPod,
    (31 to 33) -> MorphsMN.novacrab,
    (34 to 35) -> MorphsDF.digger,
    (36 to 38) -> MorphsOR.ripwing,
    (39 to 39) -> MorphsS.scurrier,
    (40 to 40) -> MorphsTX.whiplash,
    (41 to 42) -> MorphsAC.chickcharnie,
    (43 to 44) -> MorphsGL.hypergibbon,
    (45 to 46) -> MorphsS.shaper,
    (47 to 53) -> MorphsAC.ayah,
    (54 to 62) -> MorphsAC.basicPod,
    (63 to 67) -> MorphsAC.critter,
    (68 to 70) -> MorphsDF.flyingSquid,
    (71 to 72) -> MorphsGL.jenkin,
    (73 to 75) -> MorphsS.samsa,
    (76 to 83) -> MorphsS.securityPod,
    (84 to 86) -> MorphsS.spaceMarine,
    (87 to 95) -> MorphsS.specialistPod,
    (96 to 100) -> MorphsTX.vacuumPod
  );
  lazy val synthmorphs: RollTable[MorphModel] = RollTable(
    (1 to 20) -> MorphsAC.`case`,
    (21 to 35) -> MorphsS.synth,
    (36 to 40) -> MorphsAC.arachnoid,
    (41 to 45) -> MorphsDF.dragonfly,
    (46 to 49) -> MorphsDF.flexbot,
    (50 to 50) -> MorphsOR.reaper,
    (51 to 54) -> MorphsS.slitheroid,
    (55 to 58) -> MorphsS.swarmanoid,
    (59 to 59) -> MorphsOR.qMorph,
    (60 to 61) -> MorphsS.steelMorph,
    (62 to 62) -> MorphsS.steelMaskedMorph,
    (63 to 63) -> MorphsS.steelLiquidSilverMorph,
    (64 to 64) -> MorphsS.sundiver,
    (65 to 65) -> MorphsAC.cetus,
    (66 to 66) -> MorphsAC.courier,
    (67 to 67) -> MorphsDF.fenrir,
    (68 to 68) -> MorphsS.savant,
    (69 to 69) -> MorphsGL.kite,
    (70 to 70) -> MorphsS.spare,
    (71 to 72) -> MorphsTX.xuFu,
    (73 to 74) -> MorphsGL.gargoyle,
    (75 to 75) -> MorphsS.skulkerSwarmanoid,
    (76 to 77) -> MorphsTX.takko,
    (78 to 78) -> MorphsAC.biocore,
    (79 to 80) -> MorphsAC.blackbird,
    (81 to 81) -> MorphsAC.cloudSkimmer,
    (82 to 82) -> MorphsDF.daitya,
    (83 to 83) -> MorphsDF.fightingKite,
    (84 to 85) -> MorphsGL.galatea,
    (86 to 86) -> MorphsGL.griefer,
    (87 to 88) -> MorphsGL.guard,
    (89 to 89) -> MorphsGL.deluxeGuard,
    (90 to 90) -> MorphsMN.mimic,
    (91 to 91) -> MorphsMN.nautiloid,
    (92 to 93) -> MorphsOR.opteryx,
    (94 to 95) -> MorphsOR.rover,
    (96 to 96) -> MorphsOR.roverSpaceFighter,
    (97 to 97) -> MorphsS.smartSwarmanoid,
    (98 to 99) -> MorphsS.sphere,
    (100 to 100) -> MorphsS.synthtaur
  );
  lazy val infomorphs: RollTable[MorphModel] = RollTable(
    (1 to 50) -> MorphsGL.infomorph,
    (51 to 57) -> MorphsGL.agent,
    (58 to 70) -> MorphsGL.digimorph,
    (71 to 74) -> MorphsGL.elite,
    (75 to 81) -> MorphsGL.hotShot,
    (82 to 85) -> MorphsGL.sage,
    (86 to 92) -> MorphsGL.scholar,
    (93 to 93) -> MorphsGL.slave,
    (94 to 100) -> MorphsGL.wirehead
  );
}

package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ MorphModel }
import com.lkroll.ep.compendium.data._

sealed trait Resleeve;
object Resleeve {
  case object None extends Resleeve;
  case class NewMorph(model: MorphModel) extends Resleeve;
}

sealed trait MorphChoice;
object MorphChoice {
  case object Original extends MorphChoice;
  case object Uplift extends MorphChoice;
  case object Bio extends MorphChoice;
  case object Pod extends MorphChoice;
  case object Synth extends MorphChoice;
  case object Info extends MorphChoice;
}

class ResleeveTable(
  val archetype:     Archetype,
  val startingMorph: MorphModel,
  val background:    String,
  val age:           Int,
  val isAsync:       Boolean) extends Table {
  import Implicits.constToRollTable;

  override type Result = Resleeve;

  override def label: String = "Archetype Focus";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    import MorphChoice._;

    if (mustResleeve(rand)) {
      pickMorphType(rand) match {
        case Original => Resleeve.NewMorph(startingMorph)
        case Uplift => {
          val res = upliftData.randomElement(rand).get;
          Resleeve.NewMorph(res)
        }
        case Bio => {
          val res = biomorphData.randomElement(rand).get;
          Resleeve.NewMorph(res)
        }
        case Pod => {
          val res = podData.randomElement(rand).get;
          Resleeve.NewMorph(res)
        }
        case Synth => {
          val res = synthmorphData.randomElement(rand).get;
          Resleeve.NewMorph(res)
        }
        case Info => {
          val res = infomorphData.randomElement(rand).get;
          Resleeve.NewMorph(res)
        }
      }
    } else {
      Resleeve.None
    }
  }

  private def mustResleeve(rand: Random): Boolean = {
    val earthBorn = background.toLowerCase().contains("earth");
    val fallDeathProb: Int = if (age < 10) {
      0
    } else {
      archetype match {
        case Archetype.Butterfly if earthBorn                     => 96
        case Archetype.Butterfly if !earthBorn                    => 30
        case Archetype.Fighter if earthBorn                       => 99
        case Archetype.Fighter if !earthBorn                      => 70
        case Archetype.Hacker | Archetype.Scientist if earthBorn  => 98
        case Archetype.Hacker | Archetype.Scientist if !earthBorn => 40
      }
    };
    val postFallDeathProb: Int = archetype match {
      case Archetype.Butterfly                    => 10
      case Archetype.Fighter                      => 50
      case Archetype.Hacker | Archetype.Scientist => 20
    };
    val deathProb = Math.min(100, fallDeathProb + postFallDeathProb);
    val roll = rand.nextInt(100);
    roll < 100
  };

  private def pickMorphType(rand: Random): MorphChoice = {
    import MorphChoice._;

    val isAGI = background.toLowerCase().contains("agi");
    val isUplift = background.toLowerCase().contains("uplift");

    val table: RollTableLike[MorphChoice] = archetype match {
      case Archetype.Butterfly if isAGI => RollTable(
        (1 to 20) -> Original,
        (21 to 21) -> Uplift,
        (22 to 30) -> Bio,
        (31 to 35) -> Pod,
        (35 to 80) -> Synth,
        (81 to 100) -> Info)
      case Archetype.Butterfly if isUplift => RollTable(
        (1 to 70) -> Original,
        (71 to 72) -> Uplift,
        (73 to 80) -> Bio,
        (81 to 85) -> Pod,
        (86 to 99) -> Synth,
        (100 to 100) -> Info)
      case Archetype.Butterfly => RollTable(
        (1 to 50) -> Original,
        (51 to 51) -> Uplift,
        (52 to 80) -> Bio,
        (81 to 82) -> Pod,
        (83 to 99) -> Synth,
        (100 to 100) -> Info)
      case Archetype.Fighter if isAGI => RollTable(
        (1 to 5) -> Original,
        (6 to 7) -> Uplift,
        (8 to 10) -> Bio,
        (11 to 20) -> Pod,
        (21 to 95) -> Synth,
        (96 to 100) -> Info)
      case Archetype.Fighter if isUplift => RollTable(
        (1 to 70) -> Original,
        (71 to 73) -> Uplift,
        (74 to 78) -> Bio,
        (79 to 85) -> Pod,
        (86 to 99) -> Synth,
        (100 to 100) -> Info)
      case Archetype.Fighter => RollTable(
        (1 to 20) -> Original,
        (21 to 26) -> Uplift,
        (27 to 40) -> Bio,
        (41 to 60) -> Pod,
        (61 to 99) -> Synth,
        (100 to 100) -> Info)
      case Archetype.Hacker if isAGI => RollTable(
        (1 to 60) -> Original,
        (61 to 61) -> Uplift,
        (62 to 62) -> Bio,
        (63 to 65) -> Pod,
        (66 to 75) -> Synth,
        (76 to 100) -> Info)
      case Archetype.Hacker if isUplift => RollTable(
        (1 to 50) -> Original,
        (51 to 51) -> Uplift,
        (52 to 53) -> Bio,
        (54 to 60) -> Pod,
        (61 to 75) -> Synth,
        (76 to 100) -> Info)
      case Archetype.Hacker => RollTable(
        (1 to 30) -> Original,
        (31 to 31) -> Uplift,
        (32 to 50) -> Bio,
        (51 to 60) -> Pod,
        (61 to 75) -> Synth,
        (76 to 100) -> Info)
      case Archetype.Scientist if isAGI => RollTable(
        (1 to 50) -> Original,
        (51 to 51) -> Uplift,
        (52 to 56) -> Bio,
        (57 to 60) -> Pod,
        (61 to 75) -> Synth,
        (76 to 100) -> Info)
      case Archetype.Scientist if isUplift => RollTable(
        (1 to 70) -> Original,
        (71 to 71) -> Uplift,
        (72 to 73) -> Bio,
        (74 to 80) -> Pod,
        (81 to 90) -> Synth,
        (91 to 100) -> Info)
      case Archetype.Scientist => RollTable(
        (1 to 30) -> Original,
        (31 to 35) -> Uplift,
        (36 to 70) -> Bio,
        (71 to 80) -> Pod,
        (81 to 90) -> Synth,
        (91 to 100) -> Info)
    };
    table.randomElement(rand).get
  }

  lazy val biomorphData: RollTable[MorphModel] = {
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 5) -> MorphsDF.flat,
        (6 to 20) -> MorphsS.splicer,
        (21 to 40) -> MorphsDF.exalt,
        (41 to 45) -> MorphsMN.menton,
        (46 to 50) -> MorphsOR.olympian,
        (51 to 70) -> MorphsS.sylph,
        (71 to 72) -> MorphsDF.futura,
        (73 to 75) -> MorphsGL.lunarFlyer,
        (76 to 80) -> MorphsTX.venusianGlider,
        (81 to 82) -> MorphsDF.faust,
        (83 to 100) -> MorphsOR.observer)
      case Archetype.Fighter => RollTable(
        (1 to 5) -> MorphsDF.flat,
        (6 to 15) -> MorphsS.splicer,
        (16 to 23) -> MorphsDF.exalt,
        (24 to 40) -> MorphsOR.olympian,
        (41 to 50) -> MorphsAC.bouncer,
        (51 to 60) -> MorphsDF.fury,
        (61 to 65) -> MorphsGL.ghost,
        (66 to 70) -> MorphsOR.remade,
        (71 to 75) -> MorphsMN.martianAlpiner,
        (76 to 77) -> MorphsGL.hazer,
        (78 to 83) -> MorphsAC.crasher,
        (84 to 95) -> MorphsAC.bruiser,
        (96 to 97) -> MorphsMN.nomad,
        (98 to 100) -> MorphsTX.theseus)
      case Archetype.Hacker => RollTable(
        (1 to 5) -> MorphsDF.flat,
        (6 to 50) -> MorphsMN.menton,
        (51 to 80) -> MorphsGL.hyperbright,
        (81 to 90) -> MorphsGL.grey,
        (91 to 100) -> MorphsOR.observer)
      case Archetype.Scientist => RollTable(
        (1 to 5) -> MorphsDF.flat,
        (6 to 15) -> MorphsS.splicer,
        (16 to 23) -> MorphsDF.exalt,
        (24 to 35) -> MorphsMN.menton,
        (36 to 40) -> MorphsOR.olympian,
        (41 to 50) -> MorphsAC.bouncer,
        (51 to 51) -> MorphsDF.futura,
        (52 to 60) -> MorphsOR.ruster,
        (61 to 66) -> MorphsGL.hazer,
        (67 to 70) -> MorphsGL.hyperbright,
        (71 to 75) -> MorphsAC.crasher,
        (76 to 76) -> MorphsAC.ariel,
        (77 to 78) -> MorphsDF.faust,
        (79 to 82) -> MorphsGL.grey,
        (83 to 95) -> MorphsOR.observer,
        (96 to 100) -> MorphsTX.theseus)
    }
  }

  lazy val upliftData: RollTable[MorphModel] = {
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 70) -> MorphsMN.neoAvian,
        (71 to 50) -> MorphsMN.neoHominid)
      case Archetype.Fighter => RollTable(
        (1 to 50) -> MorphsMN.neoHominid,
        (51 to 60) -> MorphsOR.octomorph,
        (61 to 65) -> MorphsMN.neoNeanderthal,
        (66 to 95) -> MorphsMN.neoGorilla,
        (96 to 100) -> MorphsMN.neoPig)
      case Archetype.Hacker | Archetype.Scientist => RollTable(
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
        (100 to 100) -> MorphsMN.neoWhale)
    }
  }

  lazy val podData: RollTable[MorphModel] = {
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 40) -> MorphsOR.pleasurePod,
        (41 to 50) -> MorphsGL.hypergibbon,
        (51 to 100) -> MorphsS.shaper)
      case Archetype.Fighter => RollTable(
        (1 to 12) -> MorphsTX.workerPod,
        (13 to 15) -> MorphsMN.novacrab,
        (16 to 25) -> MorphsGL.hypergibbon,
        (26 to 30) -> MorphsS.samsa,
        (31 to 70) -> MorphsS.securityPod,
        (71 to 80) -> MorphsS.spaceMarine,
        (81 to 90) -> MorphsS.specialistPod,
        (91 to 100) -> MorphsTX.vacuumPod)
      case Archetype.Hacker => RollTable(
        (1 to 12) -> MorphsMN.novacrab,
        (13 to 15) -> MorphsS.scurrier,
        (16 to 20) -> MorphsAC.chickcharnie,
        (21 to 40) -> MorphsGL.hypergibbon,
        (41 to 45) -> MorphsAC.critter,
        (46 to 50) -> MorphsDF.flyingSquid,
        (51 to 80) -> MorphsS.specialistPod,
        (81 to 100) -> MorphsTX.vacuumPod)
      case Archetype.Scientist => RollTable(
        (1 to 20) -> MorphsDF.digger,
        (21 to 30) -> MorphsOR.ripwing,
        (31 to 35) -> MorphsS.scurrier,
        (36 to 45) -> MorphsAC.chickcharnie,
        (46 to 60) -> MorphsGL.hypergibbon,
        (61 to 65) -> MorphsDF.flyingSquid,
        (66 to 95) -> MorphsS.specialistPod,
        (96 to 100) -> MorphsTX.vacuumPod)
    }
  }

  lazy val synthmorphData: RollTable[MorphModel] = {
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 5) -> MorphsAC.`case`,
        (6 to 10) -> MorphsS.synth,
        (11 to 30) -> MorphsS.steelMaskedMorph,
        (31 to 40) -> MorphsS.savant,
        (41 to 60) -> MorphsGL.galatea,
        (61 to 70) -> MorphsGL.griefer,
        (71 to 95) -> MorphsGL.guard,
        (96 to 100) -> MorphsGL.deluxeGuard)
      case Archetype.Fighter => RollTable(
        (1 to 2) -> MorphsAC.`case`,
        (3 to 10) -> MorphsS.synth,
        (11 to 12) -> MorphsOR.reaper,
        (13 to 20) -> MorphsS.slitheroid,
        (21 to 30) -> MorphsS.steelMorph,
        (31 to 35) -> MorphsS.steelMaskedMorph,
        (36 to 40) -> MorphsS.steelLiquidSilverMorph,
        (41 to 45) -> MorphsTX.takko,
        (46 to 55) -> MorphsDF.fightingKite,
        (56 to 80) -> MorphsGL.guard,
        (81 to 95) -> MorphsGL.deluxeGuard,
        (96 to 100) -> MorphsS.synthtaur)
      case Archetype.Hacker => RollTable(
        (1 to 1) -> MorphsAC.`case`,
        (2 to 10) -> MorphsS.synth,
        (11 to 12) -> MorphsAC.arachnoid,
        (13 to 15) -> MorphsDF.dragonfly,
        (16 to 22) -> MorphsS.slitheroid,
        (23 to 28) -> MorphsS.swarmanoid,
        (29 to 35) -> MorphsS.steelMorph,
        (36 to 36) -> MorphsS.steelMaskedMorph,
        (37 to 37) -> MorphsS.steelLiquidSilverMorph,
        (38 to 45) -> MorphsS.savant,
        (46 to 50) -> MorphsGL.kite,
        (51 to 60) -> MorphsGL.gargoyle,
        (61 to 61) -> MorphsS.skulkerSwarmanoid,
        (62 to 63) -> MorphsTX.takko,
        (64 to 66) -> MorphsAC.blackbird,
        (67 to 69) -> MorphsGL.griefer,
        (70 to 73) -> MorphsMN.mimic,
        (74 to 85) -> MorphsOR.opteryx,
        (86 to 90) -> MorphsS.smartSwarmanoid,
        (91 to 100) -> MorphsS.sphere)
      case Archetype.Scientist => RollTable(
        (1 to 5) -> MorphsAC.`case`,
        (6 to 15) -> MorphsS.synth,
        (16 to 20) -> MorphsS.slitheroid,
        (21 to 30) -> MorphsS.steelMorph,
        (31 to 35) -> MorphsS.steelMaskedMorph,
        (36 to 40) -> MorphsS.steelLiquidSilverMorph,
        (41 to 55) -> MorphsS.savant,
        (56 to 60) -> MorphsGL.kite,
        (61 to 65) -> MorphsGL.gargoyle,
        (66 to 70) -> MorphsTX.takko,
        (71 to 73) -> MorphsAC.biocore,
        (74 to 76) -> MorphsAC.blackbird,
        (77 to 80) -> MorphsGL.galatea,
        (81 to 90) -> MorphsOR.opteryx,
        (91 to 100) -> MorphsS.sphere)
    }
  }

  lazy val infomorphData: RollTable[MorphModel] = {
    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 50) -> MorphsGL.infomorph,
        (51 to 60) -> MorphsGL.agent,
        (61 to 70) -> MorphsGL.elite,
        (71 to 80) -> MorphsGL.sage,
        (81 to 100) -> MorphsGL.scholar)
      case Archetype.Fighter => RollTable(
        (1 to 50) -> MorphsGL.infomorph,
        (51 to 57) -> MorphsGL.agent,
        (58 to 70) -> MorphsGL.digimorph,
        (71 to 74) -> MorphsGL.elite,
        (75 to 81) -> MorphsGL.hotShot,
        (82 to 85) -> MorphsGL.sage,
        (86 to 92) -> MorphsGL.scholar,
        (94 to 100) -> MorphsGL.wirehead)
      case Archetype.Hacker => RollTable(
        (1 to 50) -> MorphsGL.infomorph,
        (51 to 60) -> MorphsGL.elite,
        (61 to 80) -> MorphsGL.hotShot,
        (81 to 100) -> MorphsGL.wirehead)
      case Archetype.Scientist => RollTable(
        (1 to 50) -> MorphsGL.infomorph,
        (51 to 60) -> MorphsGL.elite,
        (61 to 75) -> MorphsGL.sage,
        (76 to 100) -> MorphsGL.scholar)
    }

  }
}

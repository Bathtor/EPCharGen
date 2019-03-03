package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen.utils._

case class Faction(label: String, pkg: FactionPackage)

sealed trait Allegiance;
object Allegiance {
  case object Anarchist extends Allegiance;
  case object Argonaut extends Allegiance;
  case object Martian extends Allegiance;
  case object Belter extends Allegiance;
  case object Brinker extends Allegiance;
  case object Criminal extends Allegiance;
  case object Europan extends Allegiance;
  case object BeyondHumanity extends Allegiance;
  case object Extropian extends Allegiance;
  case object Hypercorp extends Allegiance;
  case object Jovian extends Allegiance;
  case object Mercurial extends Allegiance;
  case object Orbital extends Allegiance;
  case object Ringer extends Allegiance;
  case object Scum extends Allegiance;
  case object Mercurian extends Allegiance;
  case object Socialite extends Allegiance;
  case object Solarian extends Allegiance;
  case object Titanian extends Allegiance;
  case object Ultimate extends Allegiance;
  case object Venusian extends Allegiance;
}

class FactionTable(
  val archetype:    Archetype,
  val factionLevel: PackageLevel,
  val allegiance:   Option[Allegiance] = None) extends Table {
  import Implicits.constToRollTable;

  assert(factionLevel != PackageLevel.Formative, "Faction only comes in 1PP and 3PP!");

  override type Result = Faction;

  override def label: String = "Archetype Faction";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    allegiance match {
      case Some(a) => rollFromAllegiance(rand, a)
      case None    => rollFromData(rand)
    }
  }

  private def rollFromAllegiance(rand: Random, allegiance: Allegiance): Result = {
    import Allegiance._;
    import FactionPackages._;

    val table: RollTableLike[TwoPackageGroup[FactionPackage]] = allegiance match {
      case Anarchist => RollTable(
        (1 to 70) -> anarchist,
        (71 to 75) -> barsoomian,
        (76 to 85) -> extropian,
        (86 to 88) -> ringer,
        (89 to 95) -> scum,
        (96 to 96) -> skimmer,
        (97 to 100) -> titanian)
      case Argonaut => RollTable(
        (1 to 70) -> argonaut,
        (71 to 75) -> nanoEcologist,
        (76 to 78) -> preservationist,
        (79 to 80) -> ringer,
        (81 to 84) -> singularitySeeker,
        (85 to 85) -> solarian,
        (86 to 100) -> titanian)
      case Martian => RollTable(
        (1 to 35) -> barsoomian,
        (36 to 50) -> criminal,
        (51 to 97) -> hypercorp,
        (98 to 98) -> sapient,
        (99 to 100) -> socialite)
      case Belter => RollTable(
        (1 to 14) -> anarchist,
        (15 to 70) -> belter,
        (71 to 75) -> criminal,
        (76 to 80) -> extropian,
        (81 to 95) -> hypercorp,
        (96 to 100) -> scum)
      case Brinker => brinker
      case Criminal => RollTable(
        (1 to 80) -> criminal,
        (81 to 85) -> exhuman,
        (86 to 95) -> scum,
        (96 to 100) -> singularitySeeker)
      case Europan => europan
      case BeyondHumanity => RollTable(
        (1 to 30) -> exhuman,
        (31 to 80) -> singularitySeeker,
        (81 to 100) -> ultimate)
      case Extropian => extropian
      case Hypercorp => hypercorp
      case Jovian => RollTable(
        (1 to 30) -> bioconservative,
        (31 to 90) -> jovian,
        (91 to 98) -> preservationist,
        (99 to 100) -> reclaimer)
      case Mercurial => RollTable(
        (1 to 1) -> mercurialInfolife,
        (2 to 2) -> mercurialUplift)
      case Orbital => RollTable(
        (1 to 5) -> bioconservative,
        (6 to 20) -> criminal,
        (21 to 35) -> hypercorp,
        (36 to 40) -> lunar,
        (41 to 90) -> orbital,
        (91 to 95) -> precautionist,
        (96 to 100) -> reclaimer)
      case Ringer => ringer
      case Scum   => scum
      case Mercurian => RollTable(
        (1 to 5) -> anarchist,
        (6 to 10) -> criminal,
        (11 to 20) -> hypercorp,
        (21 to 100) -> sifter)
      case Socialite => RollTable(
        (1 to 5) -> hypercorp,
        (6 to 10) -> lunar,
        (11 to 95) -> socialite,
        (96 to 100) -> venusian)
      case Solarian => solarian
      case Titanian => RollTable(
        (1 to 5) -> anarchist,
        (6 to 15) -> argonaut,
        (16 to 99) -> titanian,
        (100 to 100) -> sapient)
      case Ultimate => ultimate
      case Venusian => RollTable(
        (1 to 5) -> anarchist,
        (6 to 15) -> criminal,
        (16 to 30) -> hypercorp,
        (31 to 100) -> venusian)
    };
    val res = table.randomElement(rand).get;
    val pkg = res.ofLevel(factionLevel).get;
    val label = if (res.label == allegiance.toString()) {
      res.label
    } else {
      s"${res.label} (${allegiance})"
    }
    Faction(label, pkg)
  }

  private def rollFromData(rand: Random): Result = {
    val res = data.randomElement(rand).get;
    val pkg = res.ofLevel(factionLevel).get;
    val label = res.label;
    Faction(label, pkg)
  }

  private lazy val data: RollTable[TwoPackageGroup[FactionPackage]] = {
    import FactionPackages._;

    archetype match {
      case Archetype.Butterfly => RollTable(
        (1 to 5) -> anarchist,
        (6 to 7) -> argonaut,
        (8 to 8) -> barsoomian,
        (9 to 10) -> belter,
        (11 to 11) -> bioconservative,
        (12 to 16) -> criminal,
        (17 to 17) -> earthSurvivor,
        (18 to 20) -> europan,
        (21 to 21) -> exhuman,
        (22 to 25) -> extropian,
        (26 to 30) -> hypercorp,
        (31 to 32) -> jovian,
        (33 to 36) -> lunar,
        (37 to 37) -> mercurialUplift,
        (38 to 38) -> nanoEcologist,
        (39 to 41) -> orbital,
        (42 to 42) -> outster,
        (43 to 43) -> precautionist,
        (44 to 44) -> preservationist,
        (45 to 45) -> reclaimer,
        (46 to 46) -> ringer,
        (47 to 49) -> sapient,
        (50 to 60) -> scum,
        (61 to 61) -> singularitySeeker,
        (62 to 80) -> socialite,
        (81 to 81) -> solarian,
        (82 to 85) -> titanian,
        (86 to 86) -> ultimate,
        (87 to 100) -> venusian)
      case Archetype.Fighter => RollTable(
        (1 to 3) -> anarchist,
        (4 to 4) -> argonaut,
        (5 to 8) -> barsoomian,
        (9 to 11) -> belter,
        (12 to 12) -> bioconservative,
        (13 to 13) -> brinker,
        (14 to 17) -> criminal,
        (18 to 19) -> earthSurvivor,
        (20 to 22) -> europan,
        (23 to 25) -> exhuman,
        (26 to 29) -> extropian,
        (30 to 35) -> hypercorp,
        (36 to 38) -> jovian,
        (39 to 41) -> lunar,
        (42 to 44) -> mercurialUplift,
        (45 to 45) -> nanoEcologist,
        (46 to 49) -> orbital,
        (50 to 50) -> outster,
        (51 to 51) -> precautionist,
        (52 to 52) -> preservationist,
        (53 to 56) -> reclaimer,
        (57 to 57) -> ringer,
        (58 to 58) -> sapient,
        (59 to 65) -> scum,
        (66 to 67) -> sifter,
        (68 to 69) -> singularitySeeker,
        (70 to 71) -> skimmer,
        (72 to 72) -> socialite,
        (73 to 73) -> solarian,
        (74 to 80) -> titanian,
        (81 to 95) -> ultimate,
        (96 to 100) -> venusian)
      case Archetype.Hacker => RollTable(
        (1 to 10) -> anarchist,
        (11 to 13) -> argonaut,
        (14 to 14) -> barsoomian,
        (15 to 15) -> belter,
        (16 to 16) -> brinker,
        (17 to 25) -> criminal,
        (26 to 26) -> earthSurvivor,
        (27 to 28) -> europan,
        (29 to 29) -> exhuman,
        (30 to 35) -> extropian,
        (36 to 39) -> hypercorp,
        (40 to 41) -> jovian,
        (42 to 45) -> lunar,
        (46 to 50) -> mercurialInfolife,
        (51 to 51) -> mercurialUplift,
        (52 to 52) -> nanoEcologist,
        (53 to 53) -> orbital,
        (54 to 54) -> outster,
        (55 to 55) -> precautionist,
        (56 to 56) -> preservationist,
        (57 to 58) -> reclaimer,
        (59 to 59) -> ringer,
        (60 to 60) -> sapient,
        (61 to 75) -> scum,
        (76 to 76) -> singularitySeeker,
        (77 to 77) -> skimmer,
        (78 to 78) -> socialite,
        (79 to 79) -> solarian,
        (80 to 93) -> titanian,
        (93 to 94) -> ultimate,
        (95 to 100) -> venusian)
      case Archetype.Scientist => RollTable(
        (1 to 5) -> anarchist,
        (6 to 20) -> argonaut,
        (21 to 21) -> barsoomian,
        (22 to 22) -> belter,
        (23 to 24) -> bioconservative,
        (25 to 26) -> brinker,
        (27 to 30) -> criminal,
        (31 to 31) -> earthSurvivor,
        (32 to 33) -> europan,
        (34 to 35) -> exhuman,
        (36 to 38) -> extropian,
        (39 to 50) -> hypercorp,
        (51 to 52) -> jovian,
        (53 to 55) -> lunar,
        (56 to 57) -> mercurialInfolife,
        (58 to 59) -> mercurialUplift,
        (60 to 62) -> nanoEcologist,
        (63 to 64) -> orbital,
        (65 to 65) -> outster,
        (66 to 67) -> precautionist,
        (68 to 69) -> preservationist,
        (70 to 71) -> reclaimer,
        (72 to 74) -> ringer,
        (75 to 76) -> sapient,
        (77 to 80) -> scum,
        (81 to 81) -> sifter,
        (82 to 84) -> singularitySeeker,
        (85 to 85) -> skimmer,
        (86 to 88) -> socialite,
        (89 to 89) -> solarian,
        (90 to 95) -> titanian,
        (96 to 96) -> ultimate,
        (97 to 100) -> venusian)
    }
  }
}

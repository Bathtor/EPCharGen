package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._

case class PostFallPath(distribution: PointDistribution, adultPath: AdultPath, factionPath: FactionPath)
case class FactionPath(kind: String, index: Int, label: String, pkg: TwoPackageGroup[FactionPackage])
case class PointDistribution(kind: String, faction: PackageLevel, focus: PackageLevel)

class PostFallPathTable(val isAGI: Boolean,
                        val isUplift: Boolean,
                        val previousPath: Option[PathGroup],
                        val fallFocus: Option[Either[ThreePackageGroup[FocusPackage], CustomizationPackage]],
                        val fallFaction: Option[TwoPackageGroup[FactionPackage]])
    extends Table {

  import PostFallPathTable._;

  override type Result = PostFallPath;

  override def label: String = "Adult Paths";
  override def source: String = "Transhuman p.60-61";
  override def roll(rand: Random): Result = {
    val points = fallFocus match {
      case Some(Right(_)) => data0.get(1).get // only Faction paragon has 1PP focus needed for customization
      case _              => data0.randomElement(rand).get
    };
    val adultPath: AdultPath = fallFocus match {
      case Some(r @ Right(_)) =>
        AdultPath(kind = "The Fall shaped your path.",
                  path = PathGroup(12, "Customization", FactionPathIndex(-1)),
                  pkg = r)
      case Some(Left(pkgGroup)) =>
        AdultPath(kind = "The Fall shaped your path.",
                  path = PathGroup(12, "Customization", FactionPathIndex(-1)),
                  pkg = Left(pkgGroup))
      case None => justFocus(rand)
    };

    val factionPath: FactionPath = fallFaction match {
      case Some(pkgGroup) => FactionPath("The Fall shaped your allegiance.", -1, "None", pkgGroup)
      case None           => justFaction(adultPath, rand)
    };
    PostFallPath(points, adultPath, factionPath)
  }

  private[lifepath] def justFaction(adultPath: AdultPath, rand: Random): FactionPath = {
    if (isAGI) {
      FactionPath("Not everyone accepts AGIs.", 14, "AGIs and Uplifts", data14AGI.randomElement(rand).get)
    } else if (isUplift) {
      FactionPath("Not everyone accepts Uplifts.", 14, "AGIs and Uplifts", data14Uplift.randomElement(rand).get)
    } else {
      val factionChoice = data2.randomElement(rand).get;
      val tableIndex: Int = factionChoice match {
        case FactionChoice.Focus =>
          adultPath.path.faction.i match {
            case -1                         => data3.randomElement(rand).get // customization in prefall path
            case i if (i >= 4) && (i <= 13) => i
            case _                          => ???
          }
        case FactionChoice.Switch => data3.randomElement(rand).get
      };
      factionFromIndex(rand, tableIndex, factionChoice.kind)
    }
  }

  private[lifepath] def justFocus(rand: Random): AdultPath = {
    val (tableIndex, pckind) = data1.randomElement(rand).get match {
      case PathChoice.Stay => {
        previousPath match {
          case Some(pg) if pg.index != 12 => (pg.index, PathChoice.Stay.kind)
          case _                          => (AdultPathTable.data1.randomElement(rand).get, "New path.")
        }
      }
      case PathChoice.Switch => {
        (AdultPathTable.data1.randomElement(rand).get, PathChoice.Switch.kind)
      }
    };
    val adultPath = AdultPathTable.postFall(nextPath = AdultPathIndex(tableIndex), isAsync = false);
    adultPath.packageFromIndex(rand, tableIndex, pckind)
  }

  private[lifepath] def justCustomization(rand: Random, isAsync: Boolean): AdultPath = {
    val adultPath = AdultPathTable.postFall(AdultPathIndex(12), isAsync);
    adultPath.packageFromIndex(rand, 12, "")
  }

  private def factionFromIndex(rand: Random, tableIndex: Int, fckind: String): FactionPath = {
    tableIndex match {
      case 4  => FactionPath(fckind, 4, "Autonomist", data4.randomElement(rand).get)
      case 5  => FactionPath(fckind, 5, "Civilian", data5.randomElement(rand).get)
      case 6  => FactionPath(fckind, 6, "Criminal", data6.randomElement(rand).get)
      case 7  => FactionPath(fckind, 7, "Elite", data7.randomElement(rand).get)
      case 8  => FactionPath(fckind, 8, "Enclaver", data8.randomElement(rand).get)
      case 9  => FactionPath(fckind, 9, "Indenture", data9.randomElement(rand).get)
      case 10 => FactionPath(fckind, 10, "Military", data10.randomElement(rand).get)
      case 11 => FactionPath(fckind, 11, "Scientist", data11.randomElement(rand).get)
      case 12 => FactionPath(fckind, 12, "Spacer", data12.randomElement(rand).get)
      case 13 => FactionPath(fckind, 13, "Techie", data13.randomElement(rand).get)
    }
  }
}

object PostFallPathTable {
  import Implicits.RandomArray;
  import FactionPackages._;

  def withFallPackages(isAGI: Boolean,
                       isUplift: Boolean,
                       previousPath: Option[PathGroup] = None,
                       fallFocus: Option[Either[ThreePackageGroup[FocusPackage], CustomizationPackage]] = None,
                       fallFaction: Option[TwoPackageGroup[FactionPackage]] = None): PostFallPathTable = {
    require(!(isAGI && isUplift), "Can't be both an AGI and an Uplift!");
    new PostFallPathTable(isAGI, isUplift, previousPath, fallFocus, fallFaction)
  }

  sealed trait PathChoice {
    def kind: String;
  }
  object PathChoice {
    case object Stay extends PathChoice {
      override def kind: String = "Stay on the path.";
    }
    case object Switch extends PathChoice {
      override def kind: String = "Switch gears.";
    }
  }

  sealed trait FactionChoice {
    def kind: String;
  }
  object FactionChoice {
    case object Focus extends FactionChoice {
      override def kind: String = "Follow the Faction Path outlined by your Focus.";
    }
    case object Switch extends FactionChoice {
      override def kind: String = "Mix it up.";
    }
  }

  private val data0: RollTable[PointDistribution] = RollTable(
    (1 to 2) -> PointDistribution("Faction paragon", PackageLevel.Influential, PackageLevel.Basic),
    (3 to 5) -> PointDistribution("Equally balanced", PackageLevel.Influential, PackageLevel.Influential),
    (6 to 7) -> PointDistribution("Defined by your actions", PackageLevel.Basic, PackageLevel.Influential),
    (8 to 10) -> PointDistribution("You get the job done", PackageLevel.Basic, PackageLevel.Formative)
  );

  private val data1: RollTable[PathChoice] = RollTable((1 to 6) -> PathChoice.Stay, (7 to 10) -> PathChoice.Switch);

  private val data2: RollTable[FactionChoice] =
    RollTable((1 to 6) -> FactionChoice.Focus, (7 to 10) -> FactionChoice.Switch);

  private val data3: RandomArray[Int] = Array(4, 5, 6, 7, 8, 9, 10, 11, 12, 13);

  private val data4: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(anarchist, argonaut, barsoomian, brinker, criminal, europan, extropian, ringer, scum, titanian);

  private val data5: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(belter, bioconservative, criminal, hypercorp, lunar, orbital, reclaimer, sifter, skimmer, titanian);

  private val data6: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(anarchist, belter, brinker, criminal, exhuman, extropian, lunar, orbital, ringer, scum);

  private val data7: RandomArray[TwoPackageGroup[FactionPackage]] = Array(bioconservative,
                                                                          brinker,
                                                                          exhuman,
                                                                          extropian,
                                                                          hypercorp,
                                                                          orbital,
                                                                          socialite,
                                                                          precautionist,
                                                                          ultimate,
                                                                          venusian);

  private val data8: RandomArray[TwoPackageGroup[FactionPackage]] = Array(bioconservative,
                                                                          extropian,
                                                                          hypercorp,
                                                                          jovian,
                                                                          lunar,
                                                                          orbital,
                                                                          socialite,
                                                                          preservationist,
                                                                          reclaimer,
                                                                          venusian);

  private val data9: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(anarchist, barsoomian, hypercorp, lunar, scum, preservationist, reclaimer, sifter, skimmer, venusian);

  private val data10: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(bioconservative, brinker, criminal, hypercorp, jovian, lunar, orbital, reclaimer, precautionist, ultimate);

  private val data11: RandomArray[TwoPackageGroup[FactionPackage]] = Array(argonaut,
                                                                           europan,
                                                                           exhuman,
                                                                           hypercorp,
                                                                           nanoEcologist,
                                                                           precautionist,
                                                                           singularitySeeker,
                                                                           solarian,
                                                                           titanian,
                                                                           venusian);

  private val data12: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(belter, brinker, criminal, extropian, outster, scum, ringer, singularitySeeker, skimmer, solarian);

  private val data13: RandomArray[TwoPackageGroup[FactionPackage]] = Array(anarchist,
                                                                           argonaut,
                                                                           barsoomian,
                                                                           extropian,
                                                                           hypercorp,
                                                                           nanoEcologist,
                                                                           sifter,
                                                                           singularitySeeker,
                                                                           titanian,
                                                                           venusian);

  private val data14AGI: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(anarchist, argonaut, brinker, criminal, europan, hypercorp, mercurialInfolife, sapient, solarian, venusian);
  private val data14Uplift: RandomArray[TwoPackageGroup[FactionPackage]] =
    Array(anarchist, argonaut, brinker, criminal, europan, hypercorp, mercurialUplift, sapient, solarian, venusian);

}

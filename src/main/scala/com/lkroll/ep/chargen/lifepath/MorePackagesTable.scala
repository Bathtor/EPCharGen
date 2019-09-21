package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages._
import com.lkroll.ep.chargen.utils._

case class ExtraPackage(prefix: String, pkg: PPPackage)

class MorePackagesTable(val previousPath: PostFallPathTable, val adultPath: AdultPath, val isAsync: Boolean)
    extends Table {

  override type Result = ExtraPackage;

  sealed trait PackageChoice;
  object PackageChoice {
    case object Focus extends PackageChoice;
    case object Faction extends PackageChoice;
    case object Customization extends PackageChoice;
  }

  private val data: RollTable[PackageChoice] = RollTable((1 to 4) -> PackageChoice.Focus,
                                                         (5 to 6) -> PackageChoice.Faction,
                                                         (7 to 10) -> PackageChoice.Customization);

  override def label: String = "More Packages";
  override def source: String = "Transhuman p.67";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get match {
      case PackageChoice.Focus => {
        val path = previousPath.justFocus(rand);
        assert(path.pkg.isLeft); // should only produce left paths
        val group = path.pkg.left.get;
        ExtraPackage(group.label, group.basicPackage)
      }
      case PackageChoice.Faction => {
        val path = previousPath.justFaction(adultPath, rand);
        val group = path.pkg;
        ExtraPackage(group.label, group.basicPackage)
      }
      case PackageChoice.Customization => {
        val path = previousPath.justCustomization(rand, isAsync);
        assert(path.pkg.isRight); // should only produce right paths
        val pkg = path.pkg.right.get;
        ExtraPackage(pkg.label, pkg)
      }
    }
  }
}
object MorePackagesTable {
  def withPostFall(previousPath: PostFallPathTable, adultPath: AdultPath, isAsync: Boolean): MorePackagesTable =
    new MorePackagesTable(previousPath, adultPath, isAsync);
}

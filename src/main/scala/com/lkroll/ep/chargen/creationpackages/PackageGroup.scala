package com.lkroll.ep.chargen.creationpackages

trait PackageGroup[P <: Package] {
  def ofLevel(level: PackageLevel): Option[P#Self];
}
object PackageGroup {
  def apply[P <: Package](label: String, basic: P, influential: P): TwoPackageGroup[P] = TwoPackageGroup(label, basic, influential);
  def apply[P <: Package](label: String, basic: P, influential: P, formative: P): ThreePackageGroup[P] = ThreePackageGroup(label, basic, influential, formative);
}

case class TwoPackageGroup[P <: Package](label: String, basic: P, influential: P) extends PackageGroup[P] {
  def basicPackage: P#Self = basic.withPrefix(label);
  def influentialPackage: P#Self = influential.withPrefix(label);

  override def ofLevel(level: PackageLevel): Option[P#Self] = {
    level match {
      case PackageLevel.Basic       => Some(basicPackage)
      case PackageLevel.Influential => Some(influentialPackage)
      case PackageLevel.Formative   => None
    }
  }
}

case class ThreePackageGroup[P <: Package](label: String, basic: P, influential: P, formative: P) extends PackageGroup[P] {
  def basicPackage: P#Self = basic.withPrefix(label);
  def influentialPackage: P#Self = influential.withPrefix(label);
  def formativePackage: P#Self = formative.withPrefix(label);

  override def ofLevel(level: PackageLevel): Option[P#Self] = {
    level match {
      case PackageLevel.Basic       => Some(basicPackage)
      case PackageLevel.Influential => Some(influentialPackage)
      case PackageLevel.Formative   => Some(formativePackage)
    }
  }

  def asFaction(implicit ev: P =:= BackgroundPackage): TwoPackageGroup[FactionPackage] = {
    TwoPackageGroup(
      label = label,
      basic = basic.asFaction,
      influential = influential.asFaction)
  }
}

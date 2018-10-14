package com.lkroll.ep.chargen.creationpackages

sealed trait PackageLevel {
  def ppCost: Int;
}
object PackageLevel {
  case object Basic extends PackageLevel {
    override def ppCost: Int = 1;
  }
  case object Influential extends PackageLevel {
    override def ppCost: Int = 3;
  }
  case object Formative extends PackageLevel {
    override def ppCost: Int = 5;
  }
}

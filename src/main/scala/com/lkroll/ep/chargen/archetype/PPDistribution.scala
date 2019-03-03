package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages.PackageLevel
import com.lkroll.ep.chargen.rendering.Renderer

case class PPDistribution(background: PackageLevel, faction: PackageLevel, focus: PackageLevel, customisation: Int = 0) {
  def render(renderer: Renderer): Unit = {
    renderer.labelled("Background", s"${background.ppCost}PP");
    renderer.newline();
    renderer.labelled("Faction", s"${faction.ppCost}PP");
    renderer.newline();
    renderer.labelled("Focus", s"${focus.ppCost}PP");
    renderer.newline();
    renderer.labelled("Customisation", s"${customisation}PP");
  }
}

object PPDistributionTable extends Table {
  import com.lkroll.ep.chargen.Implicits._;
  import PackageLevel._;

  override type Result = PPDistribution;

  val data = utils.RollTable(
    (1 to 15) -> PPDistribution(Formative, Influential, Basic, 1),
    (16 to 30) -> PPDistribution(Influential, Basic, Formative, 1),
    (31 to 45) -> PPDistribution(Formative, Basic, Influential, 1),
    (46 to 60) -> PPDistribution(Basic, Influential, Formative, 1),
    //
    (61 to 90) -> PPDistribution(Influential, Influential, Influential, 1),
    //
    (91 to 95) -> PPDistribution(Formative, Basic, Basic, 3),
    (96 to 100) -> PPDistribution(Basic, Basic, Formative, 3));

  override def label: String = "PP Distribution";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
}

package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._

import scala.language.implicitConversions

case class TraitGroup(label: String, levels: Map[Int, EPTrait]) extends Iterable[EPTrait] {
  override def iterator: Iterator[EPTrait] = levels.valuesIterator;
}

object Traits extends Table {
  override type Result = TraitGroup;

  sealed trait TraitChoice;
  object TraitChoice {
    case object PositiveEgoTrait extends TraitChoice;
    case object PositiveMorphTrait extends TraitChoice;
    case object NegativeEgoTrait extends TraitChoice;
    case object NegativeMorphTrait extends TraitChoice;
  }
  import TraitChoice._;

  implicit def traitlist2map(l: List[EPTrait]): Map[Int, EPTrait] = l.map(t => (t.cp -> t)).toMap;
  implicit def trait2group(t: EPTrait): TraitGroup = TraitGroup(t.name, List(t));

  val data: RollTable[TraitChoice] = RollTable((1 to 4) -> PositiveEgoTrait,
                                               (5 to 5) -> PositiveMorphTrait,
                                               (6 to 9) -> NegativeEgoTrait,
                                               (10 to 10) -> NegativeMorphTrait);

  val dataNegativeEgo: RollTable[TraitGroup] = RollTable(
    (1 to 2) -> TraitGroup(
      "Addiction",
      List(TraitsNegativeEP.addictionMinor, TraitsNegativeEP.addictionModerate, TraitsNegativeEP.addictionMajor)
    ),
    (3 to 4) -> TraitsNegativeTranshuman.anomalousMind,
    (5 to 5) -> TraitsNegativeEP.badLuck,
    (6 to 6) -> TraitsNegativeTranshuman.beta,
    (7 to 8) -> TraitGroup("Blacklisted", List(TraitsNegativeEP.blacklistedOther, TraitsNegativeEP.blacklistedOwn))
    // TODO finish negative traits
  );

  override def label: String = "Traits";
  override def source: String = "Transhuman p.46-48";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get match {
      case PositiveEgoTrait   => ??? // TODO traits
      case PositiveMorphTrait => ??? // TODO traits
      case NegativeEgoTrait   => dataNegativeEgo.randomElement(rand).get
      case NegativeMorphTrait => ??? // TODO traits
    }
  }
}

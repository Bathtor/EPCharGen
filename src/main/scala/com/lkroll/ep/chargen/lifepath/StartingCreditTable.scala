package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._

object StartingCreditTable extends Table {

  override type Result = Int;

  implicit def int2roll(i: Int): RollTableLike[Int] = RollConstant(i);

  private val data: RollTableLike[Int] = RollSubTables(RollTable(
    (1 to 1) -> 0,
    (2 to 2) -> 5000,
    (3 to 5) -> (Dice.`1d10` * 1000).+(10000),
    (6 to 8) -> (Dice.`1d10` * 1000).+(20000),
    (9 to 9) -> 40000,
    (10 to 10) -> 50000));

  override def label: String = "Starting Credit";
  override def source: String = "Transhuman p.72";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
}

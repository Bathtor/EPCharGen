package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.Aptitude
import com.lkroll.ep.compendium.data.TraitsNegativeEP

case class StartingAgeResult(age: Int, skipPreFall: Boolean, mods: List[CharacterMod] = Nil)

object StartingAge extends Table {
  import CharImplicits._;

  override type Result = StartingAgeResult;

  case class CharacterDecade(decade: Int, next: DecadeNext)
  sealed trait DecadeNext;
  object DecadeNext {
    case class Proceed(skipPreFall: Boolean) extends DecadeNext
    case class AdvancedAge(mod: Int) extends DecadeNext
  }

  private val data: RollTable[CharacterDecade] = RollTable(
    (1 to 20) -> CharacterDecade(20, DecadeNext.Proceed(true)),
    (21 to 50) -> CharacterDecade(30, DecadeNext.Proceed(false)),
    (51 to 70) -> CharacterDecade(40, DecadeNext.Proceed(false)),
    (71 to 80) -> CharacterDecade(50, DecadeNext.Proceed(false)),
    (81 to 85) -> CharacterDecade(60, DecadeNext.Proceed(false)),
    (86 to 90) -> CharacterDecade(70, DecadeNext.Proceed(false)),
    (91 to 94) -> CharacterDecade(80, DecadeNext.Proceed(false)),
    (95 to 98) -> CharacterDecade(90, DecadeNext.AdvancedAge(1)),
    (99 to 100) -> CharacterDecade(100, DecadeNext.AdvancedAge(0))
  );

  case class AdvancedResult(mod: Option[RollTableLike[CharacterMod]], reroll: Int = 0)

  private val advancedData: RollTable[AdvancedResult] = RollTable(
    (1 to 1) -> AdvancedResult(mod = Some(AptitudeTables.physicalMinus10), reroll = 2),
    (2 to 2) -> AdvancedResult(mod = Some(AptitudeTables.physicalMinus5), reroll = 6),
    (3 to 3) -> AdvancedResult(mod = Some(RollConstant(TraitsNegativeEP.immortalityBlues)), reroll = 0),
    (4 to 4) -> AdvancedResult(mod = Some(AptitudeTables.mentalPlus5), reroll = 3),
    (5 to 10) -> AdvancedResult(mod = None, reroll = 0)
  );

  private val advancedDataMod1: RollTable[AdvancedResult] = RollTable(
    (1 to 1) -> AdvancedResult(mod = Some(AptitudeTables.physicalMinus5), reroll = 6),
    (2 to 2) -> AdvancedResult(mod = Some(RollConstant(TraitsNegativeEP.immortalityBlues)), reroll = 0),
    (3 to 3) -> AdvancedResult(mod = Some(AptitudeTables.mentalPlus5), reroll = 3),
    (4 to 10) -> AdvancedResult(mod = None, reroll = 0)
  );

  override def label: String = "Starting Age";
  override def source: String = "Transhuman p.60";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get match {
      case CharacterDecade(decade, DecadeNext.Proceed(skipPreFall)) => {
        StartingAgeResult(decadeToAge(rand, decade), skipPreFall)
      }
      case CharacterDecade(90, DecadeNext.AdvancedAge(1)) => {
        val age = decadeToAge(rand, 90);
        var mods: List[CharacterMod] = Nil;
        var rolls = 0;
        var done = false;
        while (!done) {
          val res = advancedDataMod1.randomElement(rand).get;
          rolls += 1;
          if (res.reroll <= rolls) {
            done = true;
          }
          res.mod match {
            case Some(rm) => {
              val mod = rm.randomElement(rand).get;
              mods ::= mod;
            }
            case None => // ok
          }
        }
        StartingAgeResult(age, false, mods)
      }
      case CharacterDecade(100, DecadeNext.AdvancedAge(0)) => {
        val age = 100 + rand.nextInt(31);
        var mods: List[CharacterMod] = Nil;
        var rolls = 0;
        var done = false;
        while (!done) {
          val res = advancedData.randomElement(rand).get;
          rolls += 1;
          if (res.reroll <= rolls) {
            done = true;
          }
          res.mod match {
            case Some(rm) => {
              val mod = rm.randomElement(rand).get;
              mods ::= mod;
            }
            case None => // ok
          }
        }
        StartingAgeResult(age, false, mods)
      }
      case _ => ???
    }
  }

  private def decadeToAge(rand: Random, decade: Int): Int = {
    val years = rand.nextInt(10);
    decade + years
  }

  object AptitudeTables {
    import Aptitude._;
    import Implicits.RandomArray;

    val physicalMinus10: RollTableLike[CharacterMod] = Array[CharacterMod]((SOM - 10), (REF - 10), (COO - 10));

    val physicalMinus5: RollTableLike[CharacterMod] = Array[CharacterMod]((SOM - 5), (REF - 5), (COO - 5));

    val mentalPlus5: RollTableLike[CharacterMod] = Array[CharacterMod]((COG + 5), (INT + 5), (SAV + 5), (WIL + 5));

    def any(mod: Int): RollTableLike[CharacterMod] =
      Array[CharacterMod](COG + mod, COO + mod, INT + mod, REF + mod, SAV + mod, SOM + mod, WIL + mod);
  }
}

package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.compendium._

import scala.language.implicitConversions
import scala.reflect.ClassTag

object PackageImplicits {
  import Implicits.RandomArray;

  object Moxie {
    def +(i: Int): CharacterMod.Moxie = CharacterMod.Moxie(i);
    def -(i: Int): CharacterMod.Moxie = CharacterMod.Moxie(-i);
  }

  object StartingCredit {
    def +(i: Int): CharacterMod.CreditMod = CharacterMod.CreditMod(i);
    def -(i: Int): CharacterMod.CreditMod = CharacterMod.CreditMod(-i);
  }

  object Sleights {
    object PsiChi {
      def +(i: Int): PackageContent.RandMods = {
        val f = (rand: Random) =>
          (1 to i).map(_ => PsiChiSleights.roll(rand)).map(CharacterMod.GainPsiSleight(_)).toList;
        PackageContent.RandMods(f)
      }
    }
    object PsiGamma {
      def +(i: Int): PackageContent.RandMods = {
        val f = (rand: Random) =>
          (1 to i).map(_ => PsiGammaSleights.roll(rand)).map(CharacterMod.GainPsiSleight(_)).toList;
        PackageContent.RandMods(f)
      }
    }

    def +(i: Int): PackageContent.RandMods = {
      val f = (rand: Random) =>
        (1 to i)
          .map { _ =>
            if (rand.nextBoolean()) {
              PsiChiSleights.roll(rand)
            } else {
              PsiGammaSleights.roll(rand)
            }
          }
          .map(CharacterMod.GainPsiSleight(_))
          .toList;
      PackageContent.RandMods(f)
    }
  }

  case class PickOne[T: ClassTag](ts: T*) {
    def map(f: T => CharacterMod): PackageContent.RandMod = r(rand => f(ts.toArray.randomElement(rand).get));
    def mapPC(f: T => PackageContent.Mod): PackageContent.RandMod = rpc(rand => f(ts.toArray.randomElement(rand).get));
  }

  def r(f: Random => CharacterMod): PackageContent.RandMod = PackageContent.RandMod(f);
  def rpc(f: Random => PackageContent.Mod): PackageContent.RandMod = PackageContent.RandMod(rand => f(rand).mod);

  implicit def cmod2pcmod(mod: CharacterMod): PackageContent.Mod = PackageContent.Mod(mod);
  implicit def effect2pcmod(e: Effect.AptitudeMod): PackageContent.Mod = CharacterMod.AptitudeMod(e.apt, e.mod);
  implicit def trait2gain(t: EPTrait): PackageContent.Mod = CharacterMod.GainTrait(t);

  implicit def morphT2filter(mt: MorphType): MorphFilter = MorphFilter.MType(mt);
}

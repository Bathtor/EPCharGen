package com.lkroll.ep.chargen

import com.lkroll.ep.compendium.{ AptitudeValues, Aptitude }

object Implicits {
  implicit def int2opt(i: Int): Option[Int] = Some(i);
  implicit def str2opt(s: String): Option[String] = Some(s);
  implicit def constToRollTable[V](v: V): utils.RollTableLike[V] = utils.RollConstant(v);

  implicit class RandomArray[T](arr: Array[T]) extends utils.RollTableLike[T] {
    override def randomElement(rand: Random): Option[T] = {
      if (arr.isEmpty) {
        None
      } else {
        val index = rand.nextInt(arr.size);
        Some(arr.apply(index))
      }
    }
    override def entries: Iterable[T] = arr.toIterable;
  }

  implicit class AptitudesRendering(apts: AptitudeValues) {
    def toKV: List[(String, String)] = apts.labelledValues.map {
      case (k, v) => (k.toUpperCase() -> v.get.toString)
    };

    def getValueFor(apt: Aptitude): Option[Int] = {
      apt match {
        case Aptitude.COG => apts.cog
        case Aptitude.COO => apts.coo
        case Aptitude.INT => apts.int
        case Aptitude.REF => apts.ref
        case Aptitude.SAV => apts.sav
        case Aptitude.SOM => apts.som
        case Aptitude.WIL => apts.wil
      }
    }
    def valueFor(apt: Aptitude): Int = {
      getValueFor(apt).getOrElse(0)
    }
  }
}

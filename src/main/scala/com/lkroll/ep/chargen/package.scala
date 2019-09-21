package com.lkroll.ep

import probability_monad.{Distribution, Distributions}

import scala.language.implicitConversions

package object chargen {
  type Random = scala.util.Random;

  implicit class DistributionExt(dist: Distribution[Int]) {
    def sampleClamp(min: Int, max: Int): Int = {
      var res: Int = Int.MaxValue;
      while (res > max || res < min) {
        res = dist.sample(1).head;
      }
      res
    }
  }
}

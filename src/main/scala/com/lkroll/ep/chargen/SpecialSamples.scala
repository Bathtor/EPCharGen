package com.lkroll.ep.chargen

import probability_monad.{ Distribution, Distributions }

class SpecialSamples(rand: Random) {

  val dist = new Distributions(rand);

  def nameLength(): Int = {
    var res = Int.MaxValue;
    val geom = dist.geometric(0.9);
    while (res > 6 || res < 1) {
      res = geom.sample(1).head;
    }
    res
  }
}

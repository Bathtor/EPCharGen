package com.lkroll.ep.chargen

import probability_monad.{ Distribution, Distributions }

class SpecialSamples(rand: Random) {

  val dist = new Distributions(rand);

  def nameLength(): Int = {
    val geom = dist.geometric(0.9);
    geom.sampleClamp(1, 6)
  }
}

package com.lkroll.ep.chargen

trait RandomTest { self =>
  val seed = System.currentTimeMillis();
  println(s"${self.getClass.getName} using seed $seed"); // so it can be reused for reproducibility
  val rand = new Random(seed);
}

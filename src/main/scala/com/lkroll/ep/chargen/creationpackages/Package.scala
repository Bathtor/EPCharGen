package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.Random

trait GeneralPackage {
  def label: String;
  def applyTo(c: CharGenCharacter, rand: Random): CharGenCharacter;
}

trait PPPackage extends GeneralPackage {
  def ppCost: Int;
}

trait GroupedPackage extends PPPackage {
  type Self;

  def withPrefix(prefix: String): Self;
}

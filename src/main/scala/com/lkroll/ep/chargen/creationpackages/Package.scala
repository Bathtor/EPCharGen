package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.Random

trait Package {
  type Self;

  def label: String;
  def withPrefix(prefix: String): Self;
  def ppCost: Int;
  def applyTo(c: Character, rand: Random): Character;
}

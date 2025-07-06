package com.lkroll.ep.chargen

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class SpecialSamplesTest extends AnyFunSuite with Matchers with RandomTest {

  val specialSamples = new SpecialSamples(rand);

  test("Name length should return random numbers") {
    for (_ <- 1 to 100) {
      val res = specialSamples.nameLength();
      res should be <= 6;
      res should be >= 1;
    }
  }
}

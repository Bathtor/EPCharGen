package com.lkroll.ep.chargen.utils

import com.lkroll.ep.chargen.RandomTest
import org.scalatest._

class RollTableTest extends FlatSpec with Matchers with RandomTest with OptionValues {

  val tries = 100;
  def repeat[T](f: => T): Unit = {
    val ff = f _;
    for (_ <- 1 to tries) { ff(); };
  }

  for (n <- 1 to 2; d <- 4 to 10) {
    val dice = Die(n, d);
    val max = n * d;
    s"${n}d${d}" should "produce numbers within their bounds" in {
      repeat {
        val res = dice.randomElement(rand);
        res.value should be >= n;
        res.value should be <= max;
      }
    }
  }

  "A RollConstant" should "produce itself" in {
    val rc = RollConstant(seed);
    repeat {
      val res = rc.randomElement(rand);
      res.value shouldEqual (seed);
    }
  }

  "A RollTable" should "produce values within their ranges" in {
    val rt = RollTable(
      (1 to 5) -> "Ok",
      (6 to 10) -> "Ok");
    repeat {
      val res = rt.randomElement(rand);
      res.value shouldBe ("Ok")
    }
  }
  it should "prevent overlapping ranges" in {
    a[java.lang.IllegalArgumentException] should be thrownBy RollTable(
      (1 to 5) -> "Ok",
      (5 to 10) -> "Not Ok");
  }
}

package com.lkroll.ep.chargen.archetype

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class OptionsTest extends AnyFunSuite with Matchers {

  test("Allegiance should read from string") {
    Allegiance.list.foreach { a =>
      val s = a.toString();
      val a2 = Allegiance.fromString(s);
      a2 shouldEqual a;
    }
  }

  test("Archetype should read from string") {
    Archetype.list.foreach { a =>
      val s = a.toString();
      val a2 = Archetype.fromString(s);
      a2 shouldEqual a;
    }
  }

  test("Origin should read from string") {
    Origin.list.foreach { o =>
      val s = o.toString();
      val o2 = Origin.fromString(s);
      o2 shouldEqual o;
    }
  }
}

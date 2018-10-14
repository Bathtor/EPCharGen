package com.lkroll.ep.chargen

import com.lkroll.ep.chargen.character.Character

trait CharFilter {
  def matches(char: Character): Boolean;

  def untilMatch(f: => CreationResult): CreationResult = {
    val ff = f _;
    while (true) {
      val cres = ff();
      if (matches(cres.character)) {
        return cres;
      }
    }
    ???
  }
}

object CharFilter {
  case object Accept extends CharFilter {
    override def matches(char: Character): Boolean = true;
  }

  case class Faction(s: String) extends CharFilter {
    override def matches(char: Character): Boolean = char.faction.contains(s);
  }

  case class ApparentGender(s: String) extends CharFilter {
    override def matches(char: Character): Boolean = char.activeMorph.visibleGender.exists(_.contains(s));
  }

  case class ActiveMorph(s: String) extends CharFilter {
    override def matches(char: Character): Boolean = char.activeMorph.model.contains(s);
  }

  case class BirthMorph(s: String) extends CharFilter {
    override def matches(char: Character): Boolean = char.startingMorph.name.contains(s);
  }

  case class Any(filters: List[CharFilter]) extends CharFilter {
    override def matches(char: Character): Boolean = filters.exists(f => f.matches(char));
  }

  case class All(filters: List[CharFilter]) extends CharFilter {
    override def matches(char: Character): Boolean = filters.forall(f => f.matches(char));
  }
}

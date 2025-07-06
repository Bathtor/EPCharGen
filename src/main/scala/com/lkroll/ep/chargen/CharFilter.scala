package com.lkroll.ep.chargen

import com.lkroll.ep.chargen.character.CharGenCharacter

trait CharFilter {
  def matches(char: CharGenCharacter): Boolean;

  def untilMatch(f: => CreationResult): CreationResult = {
    val ff = () => f;
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
  import Implicits._;

  case object Accept extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = true;
  }

  case class Faction(s: String) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = char.faction.contains(s);
  }

  case class Background(s: String) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = char.background.contains(s);
  }

  case class ApparentGender(s: String) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = char.activeMorph.visibleGender.exists(_.contains(s));
    // .map(_.toLowerCase).exists(g => {
    //   s.toLowerCase match {
    //     case "male" => g.contains("male") && !g.contains("female")
    //     case sl     => g.contains(sl)
    //   }
    // });
  }

  case class ActiveMorph(s: String) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = char.activeMorph.model.contains(s);
  }

  case class BirthMorph(s: String) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = char.startingMorph.name.contains(s);
  }

  case class SkillOver(skillName: String, field: Option[String] = None, minTotal: Int) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = {
      val res: Option[Boolean] = char.skills
        .find(
          s =>
            if (field.isDefined) {
              s.name.contains(skillName) && s.field.isDefined && s.field.get.contains(field.get)
            } else {
              s.name.contains(skillName)
            }
        )
        .map { s =>
          val total = char.aptitudes.total.valueFor(s.apt) + s.ranks;
          total > minTotal
        };
      res.getOrElse(false)
    }
  }

  case class Any(filters: List[CharFilter]) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = filters.exists(f => f.matches(char));
  }

  case class All(filters: List[CharFilter]) extends CharFilter {
    override def matches(char: CharGenCharacter): Boolean = filters.forall(f => f.matches(char));
  }
}

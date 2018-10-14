package com.lkroll.ep.chargen.character

import com.lkroll.ep.compendium._

object CharImplicits {
  implicit def trait2gain(t: EPTrait): CharacterMod.GainTrait = CharacterMod.GainTrait(t);
  implicit def effect2mod(e: Effect.AptitudeMod): CharacterMod.AptitudeMod = CharacterMod.AptitudeMod(e.apt, e.mod);

  implicit def skillcls2filter(cls: Skills.SkillClass.SkillClass): SkillFilter = SkillFilter.SkillClass(cls);
  implicit def skillcat2filter(cat: Skills.SkillCategory.SkillCategory): SkillFilter = SkillFilter.Category(cat);
  implicit def string2filter(s: String): SkillFilter = SkillFilter.Name(s);
  implicit def skill2filter(s: Skills.Skill): SkillFilter = SkillFilter.Name(s.name);

  implicit def string2motivation(s: String): Motivation = Motivation.parse(s).get;

  def int2mod(i: Int): String = {
    if (i < 0) {
      i.toString
    } else {
      s"+$i"
    }
  }
}

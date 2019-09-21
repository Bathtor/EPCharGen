package com.lkroll.ep.chargen.character

import com.lkroll.common.macros.Macros
import com.lkroll.ep.compendium.{Aptitude, AptitudeValues, CharacterSkill, SkillCategory, SkillClass, SkillDef}
import com.lkroll.ep.compendium.data.DefaultSkills
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages.PackageContent

case class Skill(skillDef: Skills.Skill, ranks: Int, specs: List[String] = Nil) {
  import Implicits._;

  def name: String = skillDef.name;
  def field: Option[String] = skillDef.field;
  def apt: Aptitude = skillDef.apt;

  def withField(s: String): Skill = this.copy(skillDef = skillDef.withField(s));

  def toCompendium(): CharacterSkill =
    CharacterSkill(name = name,
                   field = field,
                   cls = skillDef.cls,
                   category = skillDef.category,
                   apt = skillDef.apt,
                   noDefaulting = skillDef.skill.noDefaulting,
                   ranks = ranks,
                   specs = specs);

  def render(renderer: Renderer, apts: AptitudeValues): Unit = {
    val total = apts.valueFor(apt) + ranks;
    renderer.value(name);
    field.foreach { f =>
      renderer.field(f);
    }
    renderer.raw(" (");
    renderer.value(apt.label);
    renderer.raw(") ");
    renderer.labelled("Total", total.toString);
    renderer.raw(" (");
    renderer.labelled("Ranks", ranks.toString);
    renderer.raw(") - ");
    renderer.value(specs.mkString(", "));
  }

  def render(renderer: Renderer): Unit = {
    renderer.value(name);
    field.foreach { f =>
      renderer.field(f);
    }
    renderer.raw(" ");
    renderer.labelled("Total", s"$ranks + ${apt.label}");
    renderer.raw(" - ");
    renderer.value(specs.mkString(", "));
  }
}

sealed trait SkillFilter {
  def matches(skill: Skills.Skill): Boolean;
  def matches(skill: Skill): Boolean = matches(skill.skillDef);
  def render: String;
}
object SkillFilter {
  case class SkillClass(cls: com.lkroll.ep.compendium.SkillClass) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.cls == cls;
    override def render: String = cls.toString;
  }
  case class Category(cat: SkillCategory) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.category == cat;
    override def render: String = cat.toString;
  }
  case class Name(name: String) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.name.equalsIgnoreCase(name);
    override def render: String = name;
  }
  case class Not(filter: SkillFilter) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = !filter.matches(skill);
    override def render: String = s"not ${filter.render}";
  }
}

object Skills {
  import Implicits._;

  def chooseAny(ranks: Int): PackageContent.Skill =
    PackageContent.Skill(Right(PackageContent.SkillChoice.PickAny), Right(PackageContent.SkillChoice.PickAny), ranks);
  def chooseOnly(ranks: Int, filters: SkillFilter*): PackageContent.Skill =
    PackageContent.Skill(Right(PackageContent.SkillChoice.PickOnly(filters.toList)),
                         Right(PackageContent.SkillChoice.PickAny),
                         ranks);
  def modAny(rand: Random, mod: Int): CharacterMod.SkillMod =
    CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickAny(rand)),
                          Right(CharacterMod.SkillChoice.PickAny(rand)),
                          mod);
  def modOnly(rand: Random, mod: Int, filters: SkillFilter*): CharacterMod.SkillMod =
    CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickOnly(rand, filters.toList)),
                          Right(CharacterMod.SkillChoice.PickAny(rand)),
                          mod);
  def oneOf(skills: Skill*): PackageContent.SkillChoice.OneOf =
    PackageContent.SkillChoice.OneOf(skills.map(_.name).toList);
  def specializeAny(rand: Random): CharacterMod.SkillMod =
    CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickAny(rand)),
                          Right(CharacterMod.SkillChoice.PickAny(rand)),
                          0,
                          Some(Right(CharacterMod.SkillChoice.PickAny(rand))));

  case class Skill(skill: SkillDef) {

    def name = skill.name;
    def field = skill.field;
    def cls = skill.cls;
    def category = skill.category;
    def apt = skill.apt;
    lazy val sampleFields = skill.sampleFields.map(_.toArray);
    lazy val sampleSpecs = skill.sampleSpecs.toArray;

    def instance(ranks: Int = 0): com.lkroll.ep.chargen.character.Skill = {
      com.lkroll.ep.chargen.character.Skill(this, ranks)
    }
    def withField(s: String): Skill = this.copy(skill = skill.copy(field = Some(s)));
    def randomField(rand: Random): Option[Skill] = {
      for {
        samples <- sampleFields;
        field <- samples.randomElement(rand)
      } yield this.withField(field)
    }
    def +(mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), mod);
    def -(mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), -mod);
    def modAnyField(rand: Random, mod: Int): CharacterMod.SkillMod =
      CharacterMod.SkillMod(Left(name), Right(CharacterMod.SkillChoice.PickAny(rand)), mod);
    def gainSpecialization(spec: String): CharacterMod.SkillMod =
      CharacterMod.SkillMod(Left(name), Left(field), 0, Some(Left(spec)));
    def anySpecialization(rand: Random): CharacterMod.SkillMod =
      CharacterMod.SkillMod(Left(name), Left(field), 0, Some(Right(CharacterMod.SkillChoice.PickAny(rand))));

    def at(ranks: Int): PackageContent.Skill = PackageContent.Skill(Left(name), Left(field), ranks);
    def anyField(ranks: Int): PackageContent.Skill =
      PackageContent.Skill(Left(name), Right(PackageContent.SkillChoice.PickAny), ranks);
    def oneOf(fields: String*): PackageContent.Skill =
      PackageContent.Skill(Left(name), Right(PackageContent.SkillChoice.OneOf(fields.toList)), 0);

    def matches(other: Skill): Boolean = this.skill.matches(other.skill);
  }

  private def strArray(s: String): Array[String] = s.split(",").map(_.trim());

  object Defaults {
    val list: List[Skill] = DefaultSkills.list.map(Skill(_));
  }
}

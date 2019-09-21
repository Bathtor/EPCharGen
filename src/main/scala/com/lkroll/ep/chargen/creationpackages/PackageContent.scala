package com.lkroll.ep.chargen.creationpackages

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character._
import com.lkroll.ep.chargen.character.CharacterMod.SkillChoice

sealed trait PackageContent;
object PackageContent {
  import com.lkroll.ep.chargen.Implicits.RandomArray;

  case class Mod(mod: CharacterMod) extends PackageContent
  case class Mods(mods: List[CharacterMod]) extends PackageContent
  case class RandMod(mod: Random => CharacterMod) extends PackageContent
  case class RandMods(mods: Random => List[CharacterMod]) extends PackageContent

  case class Skill(skill: Either[String, SkillChoice], field: Either[Option[String], SkillChoice], ranks: Int) {
    def toSkill(rand: Random): character.Skill = {
      val skillTemplate: Skills.Skill = skill match {
        case Left(name) => Skills.Defaults.list.find(s => s.name.equalsIgnoreCase(name)).get
        case Right(SkillChoice.PickAny) => {
          val choice = rand.nextInt(Skills.Defaults.list.size);
          Skills.Defaults.list(choice)
        }
        case Right(SkillChoice.OneOf(skills)) => {
          val choice = rand.nextInt(skills.size);
          val name = skills(choice);
          Skills.Defaults.list.find(s => s.name.equalsIgnoreCase(name)).get
        }
        case Right(SkillChoice.PickOnly(filters)) => {
          val filtered = Skills.Defaults.list.filter(s => filters.exists(_.matches(s)));
          val choice = rand.nextInt(filtered.size);
          filtered(choice)
        }
      };
      if (skillTemplate.field.isDefined) { // requires a field
        field match {
          case Left(Some(fieldName)) => skillTemplate.withField(fieldName).instance(ranks)
          case Left(None) =>
            require(skillTemplate.field.isEmpty, "Skill requires field, but none was given."); ???
          case Right(SkillChoice.PickAny) => {
            val fieldName =
              skillTemplate.sampleFields.flatMap(samples => samples.randomElement(rand)).getOrElse("Choose One");
            skillTemplate.withField(fieldName).instance(ranks)
          }
          case Right(SkillChoice.OneOf(fields)) => {
            val choice = rand.nextInt(fields.size);
            val fieldName = fields(choice);
            skillTemplate.withField(fieldName).instance(ranks)
          }
          case Right(_: SkillChoice.PickOnly) => ??? // doesn't make sense here
        }
      } else { // just ignore the field
        skillTemplate.instance(ranks)
      }
    }

    def at(ranks: Int): Skill = this.copy(ranks = ranks);

    def matches(s: Skills.Skill): Boolean = {
      val nameMatch = skill match {
        case Left(name)                           => s.name == name
        case Right(SkillChoice.PickAny)           => true
        case Right(SkillChoice.OneOf(skills))     => skills.contains(s.name)
        case Right(SkillChoice.PickOnly(filters)) => filters.exists(_.matches(s))
      };
      if (nameMatch) {
        s.field match {
          case Some(sf) => {
            field match {
              case Left(Some(fieldName))            => sf == fieldName
              case Left(None)                       => false
              case Right(SkillChoice.PickAny)       => true
              case Right(SkillChoice.OneOf(fields)) => fields.contains(sf)
              case Right(SkillChoice.PickOnly(_))   => ??? // doesn't make sense here
            }
          }
          case None => true
        }
      } else {
        false
      }
    }
  }

  sealed trait SkillChoice;
  object SkillChoice {

    case object PickAny extends SkillChoice;
    case class OneOf(options: List[String]) extends SkillChoice {
      require(options.nonEmpty, "Empty OneOf doesn't make any sense!");
      def at(ranks: Int): Skill = Skill(Right(this), Right(PickAny), ranks);
      def +(mod: Int): CharacterMod.SkillMod =
        CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.OneOf(options)), Left(None), mod);
      def -(mod: Int): CharacterMod.SkillMod =
        CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.OneOf(options)), Left(None), -mod);
    }
    case class PickOnly(filters: List[SkillFilter]) extends SkillChoice;
  }
}

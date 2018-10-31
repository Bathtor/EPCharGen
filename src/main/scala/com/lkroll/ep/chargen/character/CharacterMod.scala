package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.compendium._
import com.typesafe.scalalogging.LazyLogging

trait CharacterMod {
  def applyTo(character: CharGenCharacter): CharGenCharacter;
  def render: String;
}
object CharacterMod extends LazyLogging {
  import Implicits.RandomArray;
  import CharImplicits.int2mod;

  case class Moxie(mod: Int) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      character.copy(moxie = character.moxie + mod)
    }
    override def render: String = s"${int2mod(mod)} Moxie";
  }
  case class GainTrait(egoTrait: EPTrait) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      character.copy(traits = (egoTrait :: character.traits))
    }
    override def render: String = s"Gained trait ${egoTrait.templateTitle}";
  }
  case class GainPsiSleight(sleight: PsiSleight) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      sleight.sleightType match {
        case SleightType.Chi     => character.copy(psiChiSleights = (sleight :: character.psiChiSleights))
        case SleightType.Gamma   => character.copy(psiGammaSleights = (sleight :: character.psiGammaSleights))
        case SleightType.Epsilon => ??? // characters probably shouldn't have these
      }
    }
    override def render: String = s"Gained ${sleight.templateSubTitle} ${sleight.templateTitle}";
  }
  case object BecomeAsync extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      character.copy(isAsync = true)
    }
    override def render: String = "Became infected with the Watts-MacLeod Strain and developed async abilities.";
  }
  case class GainGear(item: Gear) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      character.copy(gear = (GearEntry(item, 1) :: character.gear))
    }
    override def render: String = s"Gained ${item.templateSubTitle} ${item.templateTitle}";
  }
  case class AptitudeMod(apt: Aptitude, mod: Int) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      val apts = character.aptitudes.base;
      val newApts = apt match {
        case Aptitude.COG => apts.copy(cog = Some(apts.cog.getOrElse(0) + mod))
        case Aptitude.COO => apts.copy(coo = Some(apts.coo.getOrElse(0) + mod))
        case Aptitude.INT => apts.copy(int = Some(apts.int.getOrElse(0) + mod))
        case Aptitude.REF => apts.copy(ref = Some(apts.ref.getOrElse(0) + mod))
        case Aptitude.SAV => apts.copy(sav = Some(apts.sav.getOrElse(0) + mod))
        case Aptitude.SOM => apts.copy(cog = Some(apts.som.getOrElse(0) + mod))
        case Aptitude.WIL => apts.copy(cog = Some(apts.wil.getOrElse(0) + mod))
      }
      character.copy(aptitudes = character.aptitudes.copy(base = newApts))
    }
    override def render: String = s"${int2mod(mod)} ${apt.label}";
  }

  case class CreditMod(mod: Int) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      character.copy(startingCredit = character.startingCredit + mod)
    }
    override def render: String = s"${int2mod(mod)} starting credit";
  }

  case class RepMod(network: Either[RepNetwork, SkillChoice], mod: Int) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      val chosen: RepNetwork = network match {
        case Left(rn) => rn
        case Right(SkillChoice.PickAny(rand)) => {
          if (character.rep.isEmpty) {
            RepNetworks.list.toArray.randomElement(rand).get
          } else {
            if (rand.nextBoolean()) {
              character.rep.keys.toArray.randomElement(rand).get
            } else {
              RepNetworks.list.toArray.randomElement(rand).get
            }
          }
        }
        case Right(SkillChoice.OneOf(l)) => {
          val choices = l.flatMap(choice => RepNetworks.list.find(n => n.name.equalsIgnoreCase(choice)));
          choices.head // one better work
        }
        case Right(_: SkillChoice.PickOnly) => ??? // doesn't make sense here
      };
      val newVal = character.rep.getOrElse(chosen, 0) + mod;
      character.copy(rep = character.rep + (chosen -> newVal))
    }
    override def render: String = {
      network match {
        case Left(rn)                       => s"${int2mod(mod)} ${rn.name}";
        case Right(SkillChoice.PickAny(_))  => s"${int2mod(mod)} any rep";
        case Right(SkillChoice.OneOf(l))    => s"${int2mod(mod)} one rep of ${l.mkString(" or ")}";
        case Right(_: SkillChoice.PickOnly) => ??? // doesn't make sense here
      }
    }
  }

  case class SkillMod(skill: Either[String, SkillChoice], field: Either[Option[String], SkillChoice], mod: Int, specialization: Option[Either[String, SkillChoice]] = None) extends CharacterMod {
    override def applyTo(character: CharGenCharacter): CharGenCharacter = {
      require(!character.skills.isEmpty, "Cannot apply SkillMod to empty skill list!");
      val (targets, rest) = skill match {
        case Left(skillName) => {
          val res = character.skills.partition(s => s.name.equalsIgnoreCase(skillName));
          if (res._1.isEmpty) {
            Skills.Defaults.list.find(s => s.name.equalsIgnoreCase(skillName)) match {
              case Some(s) => (List(s.instance(0)), res._2)
              case None => {
                logger.warn(s"Could not find an entry for missing skill $skillName. Dropping $mod modifier.");
                res
              }
            }
          } else {
            res
          }
        }
        case Right(SkillChoice.PickAny(rand)) => choosePartition(rand, character.skills)
        case Right(SkillChoice.OneOf(l)) => {
          val res = character.skills.partition(s => l.contains(s.name));
          res._1 match {
            case Nil          => (Skills.Defaults.list.find(s => l.contains(s.name)).map(_.instance(0)).toList, character.skills)
            case _ :: Nil     => res
            case head :: rest => (List(head), rest ++ res._2)
          }
        }
        case Right(SkillChoice.PickOnly(rand, filters)) => {
          val (filtered, rest) = character.skills.partition(s => filters.exists(_.matches(s)));
          if (filtered.isEmpty) {
            (filtered, rest)
          } else {
            val part = choosePartition(rand, filtered);
            (part._1, part._2 ++ rest)
          }
        }
      };
      val (subTargets, subrest) = field match {
        case Left(Some(f)) => targets.partition(s => s.field.isDefined && s.field.get.equalsIgnoreCase(f))
        case Left(None)    => (targets, Nil)
        case Right(SkillChoice.PickAny(rand)) => {
          if (targets.isEmpty) {
            (targets, Nil) // don't make up skills...drop the mod
          } else {
            val fieldDefined = targets.map(s => if (s.field.isDefined && s.field.get == "???") {
              s.skillDef.sampleFields match {
                case Some(sampleFields) => {
                  val field = sampleFields.randomElement(rand).get;
                  s.withField(field)
                }
                case None => {
                  s.withField("Pick-A-Field")
                }
              }
            } else {
              s
            });
            choosePartition(rand, fieldDefined)
          }
        }
        case Right(SkillChoice.OneOf(l)) => {
          val res = targets.map(s => if (s.field.isDefined && s.field.get == "???") {
            s.withField(l.head)
          } else {
            s
          }).partition(s => s.field.isDefined && l.contains(s.field.get));
          res._1 match {
            case Nil          => (Nil, targets) // don't make up skills...drop the mod
            case head :: Nil  => res
            case head :: rest => (List(head), rest ++ res._2)
          }
        }
        case Right(_: SkillChoice.PickOnly) => ??? // doesn't make sense here
      };
      val updatedSkills = specialization match {
        case Some(Left(spec)) => {
          subTargets.map(s => s.copy(ranks = s.ranks + mod, specs = spec :: s.specs))
        }
        case Some(Right(SkillChoice.PickAny(rand))) => {
          subTargets.map{ s =>
            val spec = Skills.Defaults.list.find(_.name.equalsIgnoreCase(s.name)) match {
              case Some(ds) => ds.sampleSpecs.randomElement(rand).get
              case None     => "Pick-A-Spec"
            };
            s.copy(ranks = s.ranks + mod, specs = spec :: s.specs)
          }
        }
        case Some(Right(SkillChoice.OneOf(l))) => {
          val spec = l.head;
          subTargets.map(s => s.copy(ranks = s.ranks + mod, specs = spec :: s.specs))
        }
        case Some(Right(_: SkillChoice.PickOnly)) => ??? // doesn't make sense here
        case None => {
          subTargets.map(s => s.copy(ranks = s.ranks + mod))
        }
      };
      val newSkills = updatedSkills ++ subrest ++ rest;
      character.copy(skills = newSkills)
    }

    override def render: String = {
      val skillDescr = skill match {
        case Left(s)                                 => s
        case Right(SkillChoice.PickAny(_))           => s"any skill"
        case Right(SkillChoice.OneOf(l))             => s"one skill of ${l.mkString(" or ")}"
        case Right(SkillChoice.PickOnly(_, filters)) => s"any skill (${filters.map(_.render).mkString(" or ")} only)"
      };
      val fieldDescr = field match {
        case Left(Some(s))                  => ": s"
        case Left(None)                     => ""
        case Right(SkillChoice.PickAny(_))  => s": any field"
        case Right(SkillChoice.OneOf(l))    => s": one field of ${l.mkString(" or ")}"
        case Right(_: SkillChoice.PickOnly) => ??? // doesn't make sense here
      };
      s"${int2mod(mod)} $skillDescr$fieldDescr"
    }

    private def choosePartition(rand: Random, l: List[Skill]): (List[Skill], List[Skill]) = {
      require(!l.isEmpty, "Cannot partition empty skill list!");
      val len = l.length;
      val choice = rand.nextInt(len);
      var (left, right) = l.splitAt(choice);
      if (left.isEmpty) {
        right match {
          case head :: rest => {
            left ::= head;
            right = rest;
          }
          case Nil => ??? // s. above
        }
      }
      while (left.length > 1) {
        left match {
          case head :: rest => {
            left = rest;
            right ::= head;
          }
          case Nil => ??? // s. above
        }
      }
      (left, right)
    }
  }

  sealed trait SkillChoice;
  object SkillChoice {

    case class PickAny(rand: Random) extends SkillChoice;
    case class OneOf(options: List[String]) extends SkillChoice {
      require(options.nonEmpty, "Empty OneOf doesn't make any sense!");
    }
    case class PickOnly(rand: Random, filters: List[SkillFilter]) extends SkillChoice;
  }
}

package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{Aptitude, AptitudeValues, EPTrait, Motivation, RepNetwork, SkillCategory, TraitType}
import scala.collection.mutable

import scala.language.postfixOps

case class CombinationResult(char: CharGenCharacter, log: List[String])

object CombineEverything {
  import Implicits._;

  def apply(rand: Random, char: CharGenCharacter, allowMoxie: Boolean): CombinationResult = {
    var log: List[String] = Nil;
    val fixedApts =
      char.aptitudes.copy(morphBoni = char.activeMorph.aptitudeBonus, morphMax = char.activeMorph.aptitudeMax);
    log ::= "Applied Morph Apts";
    val fixedSkills = fixSkills(rand, char.skills, char.aptitudes.base);
    log ::= "Fixed Skills";

    val fixedRep = fixRep(rand, char.rep);
    log ::= "Fixed Rep";

    val finalMotivations = pickMotivations(rand, char.motivations);
    log ::= "Picked 3 Motivations";

    var (finalTraits, traitCP) = deduplicateTraits(char.traits);
    log ::= "Deduplicated Traits";
    while (traitCP < 0) {
      val (t, tcp) = pickNegativeTrait(rand, traitCP, finalTraits);
      finalTraits = t;
      traitCP = tcp;
      log ::= "Picked a negative trait to cover CP imbalance.";
    }
    val moxie = if (allowMoxie) char.moxie else 0;
    var remainingCP = traitCP + (if (allowMoxie) 0 else char.moxie * 15);
    //println(s"About to level out rep ($remainingCP CP)");
    val (finalRep, postRepCP) = levelOutRep(rand, remainingCP, fixedRep);
    log ::= "Levelled out negative rep scores to 0.";
    remainingCP = postRepCP;
    var finalSkills = fixedSkills;
    while (remainingCP > 0) {
      //println(s"About to sink CP into skills ($remainingCP CP)");
      val (skills, cp) = sinkCPIntoSkills(rand, remainingCP, finalSkills, char.aptitudes.base);
      finalSkills = skills;
      remainingCP = cp;
    }
    log ::= "Sunk remaining CP into Skills";

    val newChar = char.copy(moxie = moxie,
                            aptitudes = fixedApts,
                            rep = finalRep,
                            skills = finalSkills,
                            motivations = finalMotivations);
    CombinationResult(newChar, log.reverse)
  }

  sealed trait DuplicateSkillChoice;
  object DuplicateSkillChoice {
    case object Combine extends DuplicateSkillChoice;
    case object Switch extends DuplicateSkillChoice;
    case object SwitchAny extends DuplicateSkillChoice;
  }

  sealed trait SkillOrFieldChoice;
  case class SkillChoice(ranks: Int, cat: Option[SkillCategory]) extends SkillOrFieldChoice
  case class FieldChoice(ranks: Int, skill: Skills.Skill) extends SkillOrFieldChoice

  sealed trait SkillReductionChoice;
  object SkillReductionChoice {
    case object SplitInHalf extends SkillReductionChoice
    case object ReduceTo60 extends SkillReductionChoice
    case object SplitMax extends SkillReductionChoice
  }

  private val combineOrSwitch: RollTable[DuplicateSkillChoice] = RollTable(
    (1 to 5) -> DuplicateSkillChoice.Combine,
    (6 to 9) -> DuplicateSkillChoice.Switch,
    (10 to 10) -> DuplicateSkillChoice.SwitchAny
  );

  private val reduceSkill: RollTable[SkillReductionChoice] = RollTable((1 to 3) -> SkillReductionChoice.SplitInHalf,
                                                                       (4 to 7) -> SkillReductionChoice.ReduceTo60,
                                                                       (8 to 10) -> SkillReductionChoice.SplitMax);

  private def sinkCPIntoSkills(rand: Random, cp: Int, skills: List[Skill], apts: AptitudeValues): (List[Skill], Int) = {
    require(cp > 0);
    val (unlimited, limited) = skills.partition(s => s.ranks + apts.valueFor(s.apt) < 80);
    val (choice, rest): (Skill, List[Skill]) = if (unlimited.isEmpty) {
      pickNewSkill(rand, skills) match {
        case Some(s) => (s.instance(0), skills)
        case None    => ??? // no more skills to pick?!?
      }
    } else {
      val pos = rand.nextInt(unlimited.size);
      unlimited.splitAt(pos) match {
        case (Nil, Nil)    => ??? // da fuck?
        case (h :: r, Nil) => (h, r ::: limited)
        case (l, h :: r)   => (h, l ::: r ::: limited)
      }
    };
    val remainingSingleCPRanks = Math.max(0, 60 - (choice.ranks + apts.valueFor(choice.apt)));
    val remainingDoubleCPRanks = 80 - (choice.ranks + apts.valueFor(choice.apt)) - remainingSingleCPRanks;
    val maxCPToSpend = Math.min(cp, remainingSingleCPRanks + 2 * remainingDoubleCPRanks);
    val cpToSpend = if (maxCPToSpend == 1) 1 else rand.nextInt(maxCPToSpend - 1) + 1; // spend at least 1 to make this non-pointless
    val singleCPSpent = Math.min(cpToSpend, remainingSingleCPRanks);
    var ranks = singleCPSpent;
    var remainingCP = cpToSpend - singleCPSpent;
    if (remainingCP > 0) {
      ranks += remainingCP / 2;
      remainingCP = (remainingCP / 2) * 2;
    }
    val unspentCP = cp - (cpToSpend - remainingCP);
    val skill = choice.copy(ranks = choice.ranks + ranks);
    (skill :: rest, unspentCP)
  }

  private def pickNewSkill(rand: Random, skills: List[Skill]): Option[Skills.Skill] = {
    // this is fucking expensive, but oh well...
    val options = Skills.Defaults.list
      .flatMap(
        s =>
          s.sampleFields match {
            case Some(sampleFields) => sampleFields.map(field => s.withField(field))
            case None               => Array(s)
          }
      )
      .filter(s1 => skills.forall(s2 => !s1.matches(s2.skillDef)));
    if (options.isEmpty) {
      None
    } else {
      val pos = rand.nextInt(options.size);
      Some(options(pos));
    }
  }

  private def fixSkills(rand: Random, skills: List[Skill], apts: AptitudeValues): List[Skill] = {
    val (noduplicates, dupChoices) = removeDuplicates(rand, skills);
    var choices: List[SkillOrFieldChoice] = dupChoices;
    val limited = noduplicates.map { skill =>
      val (s, c) = limitSkill(rand, skill, apts);
      choices :::= c;
      s
    };
    var chosenSkills = limited;
    while (!choices.isEmpty) {
      val head :: rest = choices;
      val (s, c) = makeChoice(rand, head, chosenSkills, apts);
      chosenSkills ::= s;
      choices = c ::: rest;
    }

    chosenSkills
  }

  private def makeChoice(rand: Random,
                         choice: SkillOrFieldChoice,
                         skills: List[Skill],
                         apts: AptitudeValues): (Skill, List[SkillOrFieldChoice]) = {
    choice match {
      case SkillChoice(ranks, None) => {
        val options = Skills.Defaults.list.filterNot(s => skills.exists(_.name == s.name) && s.field.isEmpty);
        require(!options.isEmpty, "Character seems to have exhausted ALL skills?!?");
        val pos = rand.nextInt(options.size);
        val skillDef = options(pos);
        if (skillDef.field.isEmpty) {
          limitSkill(rand, skillDef.instance(ranks), apts)
        } else {
          skillDef.sampleFields match {
            case Some(sampleFields) => {
              val fieldOptions = sampleFields.filterNot(
                field => skills.exists(s => (s.name == skillDef.name) && (s.field.get == field))
              );
              if (fieldOptions.isEmpty) {
                limitSkill(rand, skillDef.withField("Pick-A-Field").instance(ranks), apts)
              } else {
                val field = fieldOptions.randomElement(rand).get;
                limitSkill(rand, skillDef.withField(field).instance(ranks), apts)
              }
            }
            case None => {
              limitSkill(rand, skillDef.withField("Pick-A-Field").instance(ranks), apts)
            }
          }
        }
      }
      case SkillChoice(ranks, Some(cat)) => {
        val options = Skills.Defaults.list
          .filter(_.category == cat)
          .filterNot(s => skills.exists(_.name == s.name) && s.field.isEmpty);
        if (options.isEmpty) {
          makeChoice(rand, SkillChoice(ranks, None), skills, apts)
        } else {
          val pos = rand.nextInt(options.size);
          val skillDef = options(pos);
          if (skillDef.field.isEmpty) {
            limitSkill(rand, skillDef.instance(ranks), apts)
          } else {
            skillDef.sampleFields match {
              case Some(sampleFields) => {
                val fieldOptions = sampleFields.filterNot(
                  field => skills.exists(s => (s.name == skillDef.name) && (s.field.get == field))
                );
                if (fieldOptions.isEmpty) {
                  makeChoice(rand, SkillChoice(ranks, None), skills, apts)
                } else {
                  val field = fieldOptions.randomElement(rand).get;
                  limitSkill(rand, skillDef.withField(field).instance(ranks), apts)
                }
              }
              case None => {
                limitSkill(rand, skillDef.withField("Pick-A-Field").instance(ranks), apts)
              }
            }
          }
        }
      }
      case FieldChoice(ranks, skillDef) => {
        if (skillDef.field.isEmpty) {
          limitSkill(rand, skillDef.instance(ranks), apts)
        } else {
          skillDef.sampleFields match {
            case Some(sampleFields) => {
              val fieldOptions = sampleFields.filterNot(
                field => skills.exists(s => (s.name == skillDef.name) && (s.field.get == field))
              );
              if (fieldOptions.isEmpty) {
                makeChoice(rand, SkillChoice(ranks, Some(skillDef.category)), skills, apts)
              } else {
                val field = fieldOptions.randomElement(rand).get;
                limitSkill(rand, skillDef.withField(field).instance(ranks), apts)
              }
            }
            case None => {
              limitSkill(rand, skillDef.withField("Pick-A-Field").instance(ranks), apts)
            }
          }
        }
      }
    }
  }

  private def limitSkill(rand: Random, skill: Skill, apts: AptitudeValues): (Skill, List[SkillOrFieldChoice]) = {
    val aptValue = apts.valueFor(skill.apt);
    val total = aptValue + skill.ranks;
    if (total <= 60) {
      (skill, Nil)
    } else {
      val diff = total - 60;
      val reduce = diff / 2;
      val reduced = total - reduce;
      if (reduced <= 80) {
        (skill.copy(ranks = reduced - aptValue), Nil)
      } else {
        reduceSkill.randomElement(rand).get match {
          case SkillReductionChoice.SplitInHalf => {
            val newRanks = skill.ranks / 2;
            val remainder = skill.ranks - newRanks; // in case it was uneven
            val choice = if (skill.field.isDefined) {
              FieldChoice(remainder, skill.skillDef);
            } else {
              SkillChoice(remainder, Some(skill.skillDef.category));
            };
            val (s, c) = limitSkill(rand, skill.copy(ranks = newRanks), apts);
            (s, choice :: c)
          }
          case SkillReductionChoice.ReduceTo60 => {
            val newRanks = 60 - aptValue;
            val remainder = skill.ranks - newRanks;
            val choice = if (skill.field.isDefined) {
              FieldChoice(remainder, skill.skillDef);
            } else {
              SkillChoice(remainder, Some(skill.skillDef.category));
            };
            (skill.copy(ranks = newRanks), List(choice))
          }
          case SkillReductionChoice.SplitMax => {
            val newRanks = 80 - aptValue;
            val remainder = skill.ranks - (100 - aptValue);
            val choice = if (skill.field.isDefined) {
              FieldChoice(remainder, skill.skillDef);
            } else {
              SkillChoice(remainder, Some(skill.skillDef.category));
            };
            (skill.copy(ranks = newRanks), List(choice))
          }
        }
      }
    }
  }

  private def removeDuplicates(rand: Random, skills: List[Skill]): (List[Skill], List[SkillOrFieldChoice]) = {
    val grouped = skills.groupBy(_.name);
    var choices: List[SkillOrFieldChoice] = Nil;
    val noduplicates = grouped.foldLeft(List.empty[Skill]) { (acc, e) =>
      e match {
        case (_, skill :: Nil) => skill :: acc
        case (_, sameNameSkills) =>
          if (sameNameSkills.head.field.isDefined) {
            val fieldGrouped = sameNameSkills.groupBy(_.field.get);
            val noduplicates = fieldGrouped.foldLeft(List.empty[Skill]) { (fieldAcc, fieldE) =>
              fieldE match {
                case (_, fieldSkill :: Nil) => fieldSkill :: fieldAcc
                case (_, sameFieldSkills) => {
                  val head :: rest = sameFieldSkills;
                  val fieldSkill = rest.foldLeft(head) { (sacc, s) =>
                    combineOrSwitch.randomElement(rand).get match {
                      case DuplicateSkillChoice.Combine =>
                        sacc.copy(ranks = sacc.ranks + s.ranks, specs = sacc.specs ++ s.specs)
                      case DuplicateSkillChoice.Switch => {
                        choices ::= FieldChoice(s.ranks, s.skillDef);
                        sacc.copy(specs = sacc.specs ++ s.specs)
                      }
                      case DuplicateSkillChoice.SwitchAny => {
                        choices ::= SkillChoice(s.ranks, None);
                        sacc.copy(specs = sacc.specs ++ s.specs)
                      }
                    }
                  };
                  fieldSkill :: fieldAcc
                }
              }
            };
            noduplicates ::: acc
          } else {
            val head :: rest = sameNameSkills;
            val skill = rest.foldLeft(head) { (sacc, s) =>
              combineOrSwitch.randomElement(rand).get match {
                case DuplicateSkillChoice.Combine =>
                  sacc.copy(ranks = sacc.ranks + s.ranks, specs = sacc.specs ++ s.specs)
                case DuplicateSkillChoice.Switch => {
                  choices ::= SkillChoice(s.ranks, Some(s.skillDef.category));
                  sacc.copy(specs = sacc.specs ++ s.specs)
                }
                case DuplicateSkillChoice.SwitchAny => {
                  choices ::= SkillChoice(s.ranks, None);
                  sacc.copy(specs = sacc.specs ++ s.specs)
                }
              }
            };
            skill :: acc
          }
      }
    };
    (noduplicates, choices)
  }

  private def fixRep(rand: Random, rep: Map[RepNetwork, Int]): Map[RepNetwork, Int] = {
    var assignable = 0;
    val reduced = rep.mapValues(
      v =>
        if (v > 80) {
          assignable += v - 80;
          80
        } else {
          v
        }
    );
    var assigned = reduced;
    val pickPoints = () => {
      if (assignable > 30) {
        val a = assignable / 2;
        assignable -= a;
        a
      } else {
        val a = assignable;
        assignable = 0;
        a
      }
    }
    while (assignable > 0) {
      val notLimited = assigned.filter(_._2 < 80);
      val notAssigned = RepNetworks.list.filterNot(n => assigned.contains(n));
      (notLimited.isEmpty, notAssigned.isEmpty) match {
        case (true, true) => ??? // oehm...all networks are maxed out?
        case (true, false) => {
          val pick = rand.nextInt(notAssigned.size);
          val net = notAssigned(pick);
          val amount = pickPoints();
          assigned += (net -> amount);
        }
        case (false, true) => {
          notLimited.foreach {
            case (net, cur) => {
              val left = 80 - cur;
              val amount = Math.min(left, Math.min(assignable, 10));
              assignable -= amount;
              assigned += (net -> (cur + amount));
            }
          }
        }
        case (false, false) => {
          if (rand.nextBoolean()) { // pick a new one
            val pick = rand.nextInt(notAssigned.size);
            val net = notAssigned(pick);
            val amount = pickPoints();
            assigned += (net -> amount);
          } else { // pick a fillable one
            val pick = rand.nextInt(notLimited.size);
            val (net, cur) = notLimited.toList(pick);
            val left = 80 - cur;
            val amount = Math.min(left, Math.min(assignable, 20));
            assignable -= amount;
            assigned += (net -> (cur + amount));
          }
        }
      }
    }
    assigned
  }

  private def pickMotivations(rand: Random, motivations: List[Motivation]): List[Motivation] = {
    val noduplicates = motivations
      .groupBy(_.descr)
      .map {
        case (_, Nil)          => ??? // how the?
        case (_, entry :: Nil) => entry
        case (_, entries) => {
          val choice = rand.nextInt(entries.size);
        entries(choice)
      }
    } toList;
    if (noduplicates.size == 3) {
      noduplicates
    } else if (noduplicates.size > 3) {
      var mots = noduplicates;
      while (mots.size > 3) {
        val pick = rand.nextInt(mots.size);
        mots = mots.splitAt(pick) match {
          case (Nil, Nil)           => ???
          case (Nil, head :: rest)  => rest
          case (list, Nil)          => list.reverse.tail
          case (list, head :: rest) => list ::: rest
        };
      }
      mots
    } else {
      var mots = noduplicates;
      while (mots.size < 3) {
        val res = Motivations.roll(rand);
        mots.find(m => m.descr.equalsIgnoreCase(res.descr)) match {
          case Some(_) => () // retry
          case None    => mots ::= res
        }
      }
      mots
    }
  }

  private def deduplicateTraits(traits: List[EPTrait]): (List[EPTrait], Int) = {
    val seen = mutable.Set.empty[String];
    var kept = List.empty[EPTrait];
    var cp = 0;
    traits.foreach { t =>
      if (seen.contains(t.name)) {
        t.traitType match {
          case TraitType.Negative                     => cp -= t.cp
          case TraitType.Positive | TraitType.Neutral => cp += t.cp
        }
      } else {
        kept ::= t;
      }
    }
    (kept, cp)
  }

  def pickNegativeTrait(rand: Random, cp: Int, traits: List[EPTrait]): (List[EPTrait], Int) = {
    val newTrait = Traits.dataNegativeEgo.randomElement(rand).get;
    if (traits.exists(t => t.name.startsWith(newTrait.label))) {
      (traits, cp) // No change
    } else {
      val t: EPTrait = newTrait.find(t => (t.cp + cp) > 0) match {
        case Some(t) => t
        case None    => newTrait.levels.maxBy(_._1)._2
      };
      (t :: traits, cp + t.cp)
    }
  }

  def levelOutRep(rand: Random, traitCP: Int, fixedRep: Map[RepNetwork, Int]): (Map[RepNetwork, Int], Int) = {
    var remainingCP = traitCP;
    var cpInRep: Double = fixedRep.map {
      case (_, rep) if rep > 0 => rep.toDouble / 10.0;
      case (_, rep)            => 0.0
    } sum;
    var cpToSpendOnRep = 35.0;
    var rep = fixedRep;
    while ((remainingCP > 0) && (cpInRep < cpToSpendOnRep)) {
      //println(s"Investing into rep remaining=$remainingCP, in-rep=$cpInRep, rep: $rep");
      rep.find(t => t._2 < 0) match {
        case Some((net, r)) => {
          val amount = Math.min(Math.min(Math.ceil(Math.abs(r).toDouble / 10.0).toInt * 10, remainingCP * 10),
                                ((35.0 - cpInRep) * 10).toInt);
          remainingCP -= amount / 10;
          rep += (net -> (r + amount));
          cpInRep += amount / 10;
        }
        case None => { // no negative networks remaining...can spend freeeeely
          cpToSpendOnRep = rand.nextDouble() * 34.0 + 1.0; // don't always burn all on rep
          val notLimited = rep.filter(_._2 < 80);
          val notAssigned = RepNetworks.list.filterNot(n => rep.contains(n));
          (notLimited.isEmpty, notAssigned.isEmpty) match {
            case (true, true) => cpToSpendOnRep = 0.0 // oehm...all networks are maxed out?
            case (true, false) => {
              val pick = rand.nextInt(notAssigned.size);
              val net = notAssigned(pick);
              val amount = Math.min(20, Math.min(remainingCP * 10, ((cpToSpendOnRep - cpInRep) * 10).toInt));
              remainingCP -= amount / 10;
              rep += (net -> amount);
              cpInRep += amount / 10;
            }
            case (false, true) => {
              notLimited.foreach {
                case (net, cur) => {
                  val left = 80 - cur;
                  val amount =
                    Math.min(left, Math.min(20, Math.min(remainingCP * 10, ((cpToSpendOnRep - cpInRep) * 10).toInt)));
                  remainingCP -= amount / 10;
                  rep += (net -> (cur + amount));
                  cpInRep += amount / 10;
                }
              }
            }
            case (false, false) => {
              if (rand.nextBoolean()) { // pick a new one
                val pick = rand.nextInt(notAssigned.size);
                val net = notAssigned(pick);
                val amount = Math.min(20, Math.min(remainingCP * 10, ((cpToSpendOnRep - cpInRep) * 10).toInt));
                remainingCP -= amount / 10;
                rep += (net -> amount);
                cpInRep += amount / 10;
              } else { // pick a fillable one
                val pick = rand.nextInt(notLimited.size);
                val (net, cur) = notLimited.toList(pick);
                val left = 80 - cur;
                val amount =
                  Math.min(left, Math.min(20, Math.min(remainingCP * 10, ((cpToSpendOnRep - cpInRep) * 10).toInt)));
                remainingCP -= amount / 10;
                rep += (net -> (cur + amount));
                cpInRep += amount / 10;
              }
            }
          }
        }
      }
    }

    // once we are out of CP, all we can do is zero out remaining negatives
    val finalRep = rep.mapValues(r => if (r < 0) 0 else r);
    (finalRep, remainingCP)
  }
}

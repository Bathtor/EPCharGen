package com.lkroll.ep.chargen

import java.io.{File, PrintWriter}
import org.rogach.scallop._
import com.typesafe.scalalogging.StrictLogging
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import com.lkroll.ep.chargen.names.BehindTheName
import com.lkroll.ep.chargen.impression.PersonalityTable
import com.lkroll.ep.chargen.archetype.{Allegiance, Archetype, Origin}

object Main extends StrictLogging {

  val markedPath = "/Applications/Marked 2.app/Contents/MacOS/Marked 2";
  val sublimePath = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl";

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args);
    val seed: Long = conf.seed.getOrElse(System.nanoTime());
    val rand = new Random(seed);
    val number = conf.number();
    val specialSamples = new SpecialSamples(rand);

    val filter: CharFilter = {
      var filters = List.empty[CharFilter];
      if (conf.filterFaction.isSupplied) {
        filters ::= conf.filterFaction();
      }
      if (conf.filterBackground.isSupplied) {
        filters ::= conf.filterBackground();
      }
      if (conf.filterGender.isSupplied) {
        filters ::= conf.filterGender();
      }
      if (conf.filterMorph.isSupplied) {
        filters ::= conf.filterMorph();
      }
      if (conf.filterBirthMorph.isSupplied) {
        filters ::= conf.filterBirthMorph();
      }
      if (conf.filterSkillMin.isSupplied) {
        filters :::= conf.filterSkillMin();
      }
      filters match {
        case Nil => CharFilter.Accept
        case l   => CharFilter.All(l)
      }
    };

    val firewall: FirewallOption = if (conf.firewallAlways()) {
      FirewallOption.Always
    } else if (conf.firewallAllow()) {
      FirewallOption.Allow
    } else {
      FirewallOption.Skip
    };

    val results = (1 to number)
      .map(i => {
        val cs = if (conf.lifepath()) {
          val lpc = new lifepath.LifePathCreation(fair = conf.fair(),
                                                  firewall = firewall,
                                                  gear = conf.gear(),
                                                  moxie = conf.moxie());
          lpc
        } else if (conf.archetype.isSupplied) {
          val at = conf.archetype();
          val ac = new archetype.ArchetypeCreation(archetype = at,
                                                   fair = conf.fair(),
                                                   firewall = firewall,
                                                   gear = conf.gear(),
                                                   moxie = conf.moxie(),
                                                   origin = conf.origin.toOption,
                                                   allegiance = conf.allegiance.toOption);
          ac
        } else {
          ???
        };
        val res = if (conf.seed.isSupplied || conf.deterministic()) {
          Future.successful {
            val cres = filter.untilMatch {
              cs.randomCharacter(rand)
            };
            val pTable = PersonalityTable.forChar(cres.character);
            val p = pTable.roll(rand);
            cres.copy(character = cres.character.copy(personality = Some(p)))
          }
        } else {
          Future {
            val localRand = new Random(System.nanoTime() + i);
            val cres = filter.untilMatch {
              cs.randomCharacter(localRand)
            };
            val pTable = PersonalityTable.forChar(cres.character);
            val p = pTable.roll(rand);
            cres.copy(character = cres.character.copy(personality = Some(p)))
          }
        }
        (i -> res)
      })
      .toList;

    val resultsWithNamesFs = results.map {
      case (index, charF) =>
        charF.flatMap { char =>
          BehindTheName
            .randomName(char.character.gender, char.findNativeLanguage(), specialSamples.nameLength())
            .recover {
              case t => {
                logger.error("Could not get random character name.", t);
                "Character Name"
              }
            }
            .map(name => (index -> char.withName(name, BehindTheName.source)))
        }
    };
    val resultsWithNamesF = Future.sequence(resultsWithNamesFs);
    logger.info(s"Awaiting ${resultsWithNamesFs.size} names...");
    try {
      val resultsWithNames = Await.result(resultsWithNamesF, BehindTheName.genTimeout());
      logger.info("Rendering...");
      val seedString = if (conf.deterministic() || conf.number() == 1 || conf.seed.isSupplied) {
        s"""

--- 
Random Seed: `${seed}`
"""
      } else "";
      val renderer = new rendering.MarkdownRenderer(ending = seedString);
      resultsWithNames match {
        case Nil => {
          Console.err.println("No results were produced!");
          System.exit(1);
        }
        case (_, res) :: Nil => {
          renderer.beginResult();
          res.render(renderer);
          renderer.endResult();
        }
        case moreResults => {
          moreResults.foreach {
            case (index, res) => {
              renderer.beginResult(Some(index));
              res.render(renderer);
              renderer.endResult();
            }
          }
        }
      }

      val rendered = renderer.result;
      if (conf.verbose()) {
        println(rendered);
      }
      val f = File.createTempFile("ep-compendium-macros", ".md");
      f.deleteOnExit();
      val w = new PrintWriter(f);
      w.append(rendered);
      w.flush();
      w.close();
      logger.info("Done");
      println(s"Output was generated in ${f.getAbsolutePath}");
      var procs: List[Process] = Nil;
      if (conf.sublime()) {
        val p = Runtime.getRuntime.exec(Array(sublimePath, f.getAbsolutePath));
        procs ::= p;
      }
      if (conf.marked()) {
        val p = Runtime.getRuntime.exec(Array(markedPath, f.getAbsolutePath));
        procs ::= p;
      }
      procs.foreach(_.waitFor());
      System.exit(0);
    } catch {
      case e: java.util.concurrent.TimeoutException => {
        logger.error("The command could not complete in time. Shutting down...", e);
        System.exit(1);
      }
      case e: Throwable => {
        logger.error("The command encountered an unkown error!", e);
        System.exit(1);
      }
    }
  }
}

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val verbose = opt[Boolean]("verbose", descr = "Print everything to command-line");

  val seed = opt[Long]("seed", descr = "Specify a specific random seed to use for generation");
  val marked = opt[Boolean]("marked", descr = "Open the generated file in Marked.");
  val sublime = opt[Boolean]("sublime", descr = "Open the generated file in Sublime Text.");
  val number = opt[Int]("number", default = Some(1), descr = "Generate <arg> results.");
  val deterministic = opt[Boolean](
    "deterministic",
    descr =
      "Run character generation single-threaded, to be sure that multiple characters can be regenerated with the same seed."
  );

  val filterFaction =
    opt[String]("filter-faction", descr = "Only show results where faction contains <arg>").map(CharFilter.Faction(_));
  val filterBackground = opt[String]("filter-background", descr = "Only show results where background contains <arg>")
    .map(CharFilter.Background(_));
  val filterGender = opt[String]("filter-gender", descr = "Only show results where apparent gender is <arg>")
    .map(CharFilter.ApparentGender(_));
  val filterMorph =
    opt[String]("filter-morph", descr = "Only show results wearing a <arg> model").map(CharFilter.ActiveMorph(_));
  val filterBirthMorph =
    opt[String]("filter-birth-morph", descr = "Only show results born in a <arg> model").map(CharFilter.BirthMorph(_));
  val filterSkillMin = opt[List[String]](
    "filter-skill-min",
    descr = "Only show results with a skill s at least at total n for <arg>=s>n. Can be specified multiple times."
  )(listArgConverter(identity)).map { s =>
    s.map { str =>
      //println(s"Got input $str");
      val underToSpace = str.replace("_", " ");
      val split = underToSpace.split(">");
      //println(s"Got split: ${split.mkString("'", "','", "'")}");
      require(split.size == 2, "Format for skill filters is [skillName]>[skillTotalMinimum]");
      val fieldSplit = split(0).split(":");
      //println(s"Got field split: ${fieldSplit.mkString("'", "','", "'")}");
      if (fieldSplit.size == 1) {
        CharFilter.SkillOver(skillName = split(0), minTotal = split(1).toInt)
      } else {
        CharFilter.SkillOver(skillName = fieldSplit(0), field = Some(fieldSplit(1)), minTotal = split(1).toInt)
      }
    }
  };

  val lifepath = opt[Boolean]("lifepath", descr = "Use Life Path system from Transhuman.");
  val fair = toggle(
    "fair",
    default = Some(true),
    descrYes = "Try to generate balanced characters.",
    descrNo = "Generate (potentially highly) unbalanced characters. Use only for non-combat NPCs."
  )
  val firewallAlways = opt[Boolean]("firewall-always", descr = "Always generate a Firewall event.");
  val firewallAllow = opt[Boolean](
    "firewall-allow",
    descr = "Generate a Firewall event if the history requires it (e.g., character has i-Rep or Networking: Firewall)"
  );
  val moxie = toggle(
    "moxie",
    default = Some(true),
    descrYes = "Allow generated character to have moxie.",
    descrNo = "Prevent generated characters from having moxie (for example, for NPCs)"
  );
  val gear = toggle("gear",
                    default = Some(false),
                    descrYes = "Generate gear from starting credit.",
                    descrNo = "List starting credit without generating gear.");

  val archetype = opt[String](
    "archetype",
    descr =
      s"Use Archetype system. Possible options are ${Archetype.list.map(_.toString).sorted.mkString("[", ", ", "]")}."
  ).map(Archetype.fromString);
  val origin = opt[String](
    "origin",
    descr =
      s"Fix Background to <arg>. Possible options are ${Origin.list.map(_.toString).sorted.mkString("[", ", ", "]")}."
  ).map(Origin.fromString);
  val allegiance = opt[String](
    "allegiance",
    descr =
      s"Fix Faction to <arg>. Possible options are ${Allegiance.list.map(_.toString).sorted.mkString("[", ", ", "]")}."
  ).map(Allegiance.fromString);

  requireOne(lifepath, archetype);
  dependsOnAny(fair, List(lifepath, archetype));
  dependsOnAny(firewallAlways, List(lifepath, archetype));
  dependsOnAny(firewallAllow, List(lifepath, archetype));
  mutuallyExclusive(firewallAlways, firewallAllow);
  dependsOnAny(origin, List(archetype));
  dependsOnAny(allegiance, List(archetype));

  verify()
}

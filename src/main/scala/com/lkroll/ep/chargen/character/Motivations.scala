package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium.{ Motivation, MotivationKind }
import com.typesafe.scalalogging.StrictLogging

object MotivationParser extends StrictLogging {
  import fastparse._;
  import NoWhitespace._

  def motParser[_: P]: P[Motivation] = P(kindParser ~/ ws.rep ~ nameParser).map{ case (kind, s) => Motivation(kind, s) };
  def kindParser[_: P]: P[MotivationKind] = P(eitherParser | likeParser | dislikeParser);
  def eitherParser[_: P]: P[MotivationKind] = P(("+" ~ "/" ~/ ("-" | "–")) | (("-" | "–") ~ "/" ~/ "+")).map(_ => MotivationKind.Either);
  def likeParser[_: P]: P[MotivationKind] = P("+").map(_ => MotivationKind.Like);
  def dislikeParser[_: P]: P[MotivationKind] = P("-" | "–").map(_ => MotivationKind.Dislike);
  def nameParser[_: P]: P[String] = P(AnyChar.rep.!);
  def ws[_: P]: P[Unit] = P(CharIn(" \t"));

  def parse(s: String): Option[Motivation] = {
    fastparse.parse(s, motParser(_)) match {
      case Parsed.Success(m, _) => Some(m)
      case f: Parsed.Failure => {
        logger.warn(f.msg);
        None
      }
    }
  }
}

object Motivations extends Table {
  import Implicits.RandomArray;

  override type Result = Motivation;

  val dataKind: RollTable[MotivationKind] = RollTable(
    (1 to 8) -> MotivationKind.Like,
    (9 to 10) -> MotivationKind.Dislike);

  val data: RandomArray[String] = Array(
    "Acceptance/Assimilation",
    "Alien Contact",
    "Anarchism",
    "Artistic Expression",
    "Authority/Leadership",
    "Biochauvinism",
    "Bioconservatism",
    "Destroying the TITANs",
    "DIY",
    "Education",
    "Exploration",
    "Fame",
    "Family",
    "Fascism",
    "Hard Work",
    "Hedonism",
    "Hypercapitalism",
    "Immortality",
    "Independence",
    "Individualism",
    "Law and Order",
    "Libertarianism",
    "AGI Rights",
    "Indenture Rights",
    "Infomorph Rights",
    "Pod Rights",
    "Uplift Rights",
    "Science!",
    "Self Reliance",
    "AGI Slavery",
    "Indenture Slavery",
    "Infomorph Slavery",
    "Pod Slavery",
    "Uplift Slavery",
    "Socialism",
    "Sousveillance",
    "Stability",
    "Survival",
    "Thrill Seeking",
    "Technoprogressivism",
    "Transparency",
    "Vengeance",
    "Venusian Sovereignty",
    "Vice",
    "Wealth",
    "X-Risks",
    "Skinflex",
    "Martian Liberation",
    "Morphological Freedom",
    "Nano-Ecology",
    "Neurodiversity",
    "Open Source",
    "Personal Career",
    "Personal Development",
    "Philanthropy",
    "Preservationism",
    "Reclaiming Earth",
    "Religion",
    "Research");

  override def label: String = "Motivations";
  override def source: String = "Transhuman p.41";
  override def roll(rand: Random): Result = {
    val kind = dataKind.randomElement(rand).get;
    val mot = data.randomElement(rand).get;
    Motivation(kind, mot)
  }
}

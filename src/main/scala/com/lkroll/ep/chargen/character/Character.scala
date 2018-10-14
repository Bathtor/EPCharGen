package com.lkroll.ep.chargen.character

import com.lkroll.ep.compendium._
import com.lkroll.ep.chargen.creationpackages.ChoosingAMorph
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen.Implicits.AptitudesRendering
import com.lkroll.ep.chargen.impression.Personality

case class Aptitudes(base: AptitudeValues, morphBoni: AptitudeValues, morphMax: AptitudeValues) {
  def cog: Int = total(Aptitude.COG);
  def coo: Int = total(Aptitude.COO);
  def int: Int = total(Aptitude.INT);
  def ref: Int = total(Aptitude.REF);
  def sav: Int = total(Aptitude.SAV);
  def som: Int = total(Aptitude.SOM);
  def wil: Int = total(Aptitude.WIL);

  def total: AptitudeValues = AptitudeValues(
    cog = Some(cog),
    coo = Some(coo),
    int = Some(int),
    ref = Some(ref),
    sav = Some(sav),
    som = Some(som),
    wil = Some(wil));

  private def total(apt: Aptitude): Int = {
    Math.min(morphMax.getValueFor(apt).getOrElse(20), base.valueFor(apt) + morphBoni.valueFor(apt))
  }
}

case class Character(
  name:             String,
  personality:      Option[Personality]           = None,
  gender:           GenderIdentity.GenderIdentity,
  age:              Int                           = -1,
  motivations:      List[Motivation]              = Nil,
  faction:          String                        = "None",
  aptitudes:        Aptitudes,
  moxie:            Int                           = 0,
  skills:           List[Skill],
  background:       String,
  startingMorph:    MorphModel,
  activeMorph:      MorphInstance,
  traits:           List[EPTrait]                 = Nil,
  history:          List[String]                  = Nil,
  startingCredit:   Int                           = 0,
  rep:              Map[RepNetwork, Int]          = Map.empty,
  isAsync:          Boolean                       = false,
  psiChiSleights:   List[PsiSleight]              = Nil,
  psiGammaSleights: List[PsiSleight]              = Nil,
  gear:             List[Gear]                    = Nil) {

  lazy val appearance: String = {
    val descr = {
      val parts = activeMorph.descr.split("""\+\+\+""");
      if (parts.size > 1) {
        parts(1).trim
      } else "nondescript";
    };
    val vage = activeMorph.visibleAge.map(i => s"in their ${(i / 10) * 10}s").getOrElse("of no apparent age");
    val vgen = activeMorph.visibleGender.getOrElse("genderless").toLowerCase();
    s"A $vgen ${activeMorph.model} (${activeMorph.morphType}) $vage: $descr"
  }

  def render(renderer: Renderer): Unit = {
    renderer.value(appearance);
    renderer.newline();
    personality.foreach{ p =>
      renderer.value(p.render);
      renderer.newline();
    }
    renderer.labelled("Background", background);
    renderer.newline();
    renderer.labelled("Gender Identity", gender.toString());
    renderer.newline();
    renderer.labelled("Age", age.toString());
    renderer.newline();
    renderer.labelled("Morph", s"active=${activeMorph.model}, starting=${startingMorph.name}");
    renderer.newline();
    renderer.labelled("Faction", faction);
    renderer.newline();
    renderer.labelled("Motivations", motivations.map(_.rendered).mkString(", "));
    renderer.newline();
    if (isAsync) {
      renderer.value("Character has been infected with the Watts-MacLeod virus.");
      renderer.newline();
    }
    renderer.section("Aptitudes");
    val aptBase = aptValuesToRow(aptitudes.base);
    val aptHeader = "Apts" :: aptBase._1;
    val aptBaseRow = "Base" :: aptBase._2;
    val aptMB = aptValuesToRow(aptitudes.morphBoni);
    val aptMBRow = "Morph Boni" :: aptMB._2;
    val aptMM = aptValuesToRow(aptitudes.morphMax, 20);
    val aptMMRow = "Morph Max" :: aptMM._2;
    val aptTotal = aptValuesToRow(aptitudes.total);
    val aptTotalRow = "Total" :: aptTotal._2;
    renderer.table(List(aptHeader, aptBaseRow, aptMBRow, aptMMRow, aptTotalRow));
    renderer.labelled("Moxie", moxie.toString);
    renderer.newline();
    renderer.labelled("Starting Credit", startingCredit.toString);
    renderer.section("Traits");
    renderer.list(traits.map(_.name));
    renderer.section("Skills");
    skills.sortBy(s => (s.name, s.field)).foreach { s =>
      s.render(renderer, aptitudes.total);
      renderer.newline();
    }
    renderer.section("Reputation");
    renderer.kv(rep.toList.map(t => (t._1.name, t._2.toString)).sorted);
    if (isAsync) {
      renderer.section("Psi");
      renderer.labelled("Temp Units", Math.ceil(aptitudes.wil.toDouble / 5.0).toInt.toString);
      renderer.subsection("Psi-Chi Sleights");
      renderer.list(psiChiSleights.map(_.name));
      renderer.subsection("Psi-Gamma Sleights");
      renderer.list(psiGammaSleights.map(_.name));
    } else {
      require(psiChiSleights.isEmpty && psiGammaSleights.isEmpty, "Can't have sleights if not async!");
    }
    renderer.section("Current Morph");
    renderer.value(activeMorph.templateTitle);
    renderer.note(activeMorph.templateSubTitle);
    renderer.kv(activeMorph.templateKV.toList.sortBy(_._1));
    renderer.newline();
    renderer.text(activeMorph.templateDescr);
    renderer.section("History");
    renderer.list(history);
    renderer.section("Gear");
    renderer.list(gear.map(_.name));
  }

  private def aptValuesToRow(apts: AptitudeValues, default: Int = 0): (List[String], List[String]) = {
    val vals = apts.labelledValues.map {
      case (k, v) => (k.toUpperCase() -> v.getOrElse(default).toString)
    } toList;
    vals.unzip
  }

  lazy val isAGI: Boolean = startingMorph.morphType == MorphType.Infomorph;
  lazy val isUplift: Boolean = ChoosingAMorph.uplifts.entries.map(_.name).toSet.contains(startingMorph.name);
}

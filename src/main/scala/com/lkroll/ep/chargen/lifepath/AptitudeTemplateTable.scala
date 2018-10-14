package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{AptitudeTemplate, AptitudeTemplates}

object AptitudeTemplateTable extends Table with Pickable {
  import com.lkroll.ep.chargen.Implicits.RandomArray
  
  override type Result = AptitudeTemplate;

  private val data: Array[Result] = Array(
    AptitudeTemplates.brawler,
    AptitudeTemplates.dilettante,
    AptitudeTemplates.extrovert,
    AptitudeTemplates.inquisitive,
    AptitudeTemplates.researcher,
    AptitudeTemplates.survivor,
    AptitudeTemplates.techie,
    AptitudeTemplates.thrillSeeker,
  );

  override def label: String = "Random Aptitude Template";
  override def source: String = "Transhuman p.55";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
  override def numRows: Int = data.length;
  override def pick(i: Int): Result = data(i);
}

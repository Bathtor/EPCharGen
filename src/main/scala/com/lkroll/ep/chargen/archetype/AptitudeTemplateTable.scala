package com.lkroll.ep.chargen.archetype

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.character.{AptitudeTemplate, AptitudeTemplates}

class AptitudeTemplateTable(archetype: Archetype) extends Table with Pickable {
  import com.lkroll.ep.chargen.Implicits.RandomArray

  override type Result = AptitudeTemplate;

  // Full Template Set
  //  Array(
  //    AptitudeTemplates.brawler,
  //    AptitudeTemplates.dilettante,
  //    AptitudeTemplates.extrovert,
  //    AptitudeTemplates.inquisitive,
  //    AptitudeTemplates.researcher,
  //    AptitudeTemplates.survivor,
  //    AptitudeTemplates.techie,
  //    AptitudeTemplates.thrillSeeker,
  //  )

  private val data: Array[Result] = {
    archetype match {
      case Archetype.Fighter =>
        Array(AptitudeTemplates.brawler,
              AptitudeTemplates.dilettante,
              AptitudeTemplates.survivor,
              AptitudeTemplates.thrillSeeker)
      case Archetype.Hacker =>
        Array(AptitudeTemplates.dilettante,
              AptitudeTemplates.inquisitive,
              AptitudeTemplates.researcher,
              AptitudeTemplates.techie)
      case Archetype.Scientist =>
        Array(AptitudeTemplates.dilettante,
              AptitudeTemplates.inquisitive,
              AptitudeTemplates.researcher,
              AptitudeTemplates.techie)
      case Archetype.Butterfly =>
        Array(AptitudeTemplates.dilettante, AptitudeTemplates.extrovert, AptitudeTemplates.inquisitive)
    }
  }

  override def label: String = "Random Aptitude Template by Archetype";
  override def source: String = "Homebrew";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
  override def numRows: Int = data.length;
  override def pick(i: Int): Result = data(i);
}

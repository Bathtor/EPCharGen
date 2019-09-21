package com.lkroll.ep.chargen.character

import com.lkroll.ep.compendium._

case class AptitudeTemplate(label: String, aptitudes: AptitudeValues) {}

object AptitudeTemplates {
  import com.lkroll.ep.chargen.Implicits.int2opt

  val brawler = AptitudeTemplate(
    label = "Brawler",
    aptitudes = AptitudeValues(cog = 10, coo = 20, int = 15, ref = 20, sav = 10, som = 20, wil = 10)
  );

  val dilettante = AptitudeTemplate(label = "Dilettante", aptitudes = AptitudeValues.max(15));

  val extrovert = AptitudeTemplate(
    label = "Extrovert",
    aptitudes = AptitudeValues(cog = 15, coo = 15, int = 15, ref = 15, sav = 20, som = 10, wil = 15)
  );

  val inquisitive = AptitudeTemplate(
    label = "Inquisitive",
    aptitudes = AptitudeValues(cog = 20, coo = 10, int = 20, ref = 10, sav = 20, som = 10, wil = 15)
  );

  val researcher = AptitudeTemplate(
    label = "Researcher",
    aptitudes = AptitudeValues(cog = 20, coo = 15, int = 20, ref = 15, sav = 10, som = 10, wil = 15)
  );

  val survivor = AptitudeTemplate(
    label = "Survivor",
    aptitudes = AptitudeValues(cog = 10, coo = 15, int = 15, ref = 15, sav = 10, som = 20, wil = 20)
  );

  val techie = AptitudeTemplate(label = "Techie",
                                aptitudes =
                                  AptitudeValues(cog = 20, coo = 15, int = 10, ref = 15, sav = 15, som = 15, wil = 15));

  val thrillSeeker = AptitudeTemplate(
    label = "Thrill Seeker",
    aptitudes = AptitudeValues(cog = 10, coo = 20, int = 15, ref = 20, sav = 15, som = 15, wil = 10)
  );
}

package com.lkroll.ep.chargen.character

import com.lkroll.common.macros.Macros

sealed trait Language;
abstract class LangName(val name: String) extends Language {
  override def toString = name;
}

object Languages {
  case object Afrikaans extends LangName("Afrikaans")
  case object Arabic extends LangName("Arabic")
  case object Bengali extends LangName("Bengali")
  case object Burmese extends LangName("Burmese")
  case object Cantonese extends LangName("Cantonese")
  case object Czech extends LangName("Czech")
  case object Danish extends LangName("Danish")
  case object Dutch extends LangName("Dutch")
  case object English extends LangName("English")
  case object Estonian extends LangName("Estonian")
  case object Farsi extends LangName("Farsi")
  case object Finnish extends LangName("Finnish")
  case object French extends LangName("French")
  case object German extends LangName("German")
  case object Greek extends LangName("Greek")
  case object Hebrew extends LangName("Hebrew")
  case object Hindi extends LangName("Hindi")
  case object Icelandic extends LangName("Icelandic")
  case object Indonesian extends LangName("Indonesian")
  case object Italian extends LangName("Italian")
  case object Japanese extends LangName("Japanese")
  case object Javanese extends LangName("Javanese")
  case object Khmer extends LangName("Khmer")
  case object Korean extends LangName("Korean")
  case object Kurdish extends LangName("Kurdish")
  case object Latin extends LangName("Latin")
  case object Mandarin extends LangName("Mandarin")
  case object Malay extends LangName("Malay")
  case object Norwegian extends LangName("Norwegian")
  case object Polish extends LangName("Polish")
  case object Portuguese extends LangName("Portuguese")
  case object Punjabi extends LangName("Punjabi")
  case object Romanian extends LangName("Romanian")
  case object Russian extends LangName("Russian")
  case object Serbian extends LangName("Serbian")
  case object Skandinaviska extends LangName("Skandinaviska")
  case object Slovak extends LangName("Slovak")
  case object Spanish extends LangName("Spanish")
  case object Suryan extends LangName("Suryan")
  case object Swahili extends LangName("Swahili")
  case object Swedish extends LangName("Swedish")
  case object Tagalog extends LangName("Tagalog")
  case object Tamil extends LangName("Tamil")
  case object Thai extends LangName("Thai")
  case object Turkish extends LangName("Turkish")
  case object Urdu extends LangName("Urdu")
  case object Vietnamese extends LangName("Vietnamese")
  case object Wu extends LangName("Wu")
  case object Xhosa extends LangName("Xhosa");
  case object Zulu extends LangName("Zulu");

  case object Other extends Language;

  val list: List[LangName] = Macros.memberList[LangName];

  def from(s: String): Option[Language] = {
    list.find(_.name.equalsIgnoreCase(s))
  }

}

package com.lkroll.ep.chargen.names

import com.lkroll.ep.chargen.character.{ Language, Languages }

object NameUsageCode extends Enumeration {
  type NameUsageCode = Value;

  val afk, ara, ben, bur, chi, cze, dan, dut, eng, est, fin, fre, ger, gre, grea, heb, hin, ice, ins, ira, ita, jap, jav, khm, kor, kur, mly, nor, per, pol, por, pun, rmn, roma, romm, rus, ser, slk, spa, swa, swe, tag, tam, tha, tur, urd, vie, xho, zul = Value;

  def toQuery(code: NameUsageCode): String = code.toString();
}

object LanguageUsage {
  import Languages._;
  import NameUsageCode._;

  def lookup(l: Language): List[NameUsageCode.NameUsageCode] = {
    l match {
      case Afrikaans     => List(afk)
      case Arabic        => List(ara)
      case Bengali       => List(ben)
      case Burmese       => List(bur)
      case Cantonese     => List(chi)
      case Czech         => List(cze)
      case Danish        => List(dan, nor)
      case Dutch         => List(dut)
      case English       => List(eng)
      case Estonian      => List(est)
      case Farsi         => List(per, ira)
      case Finnish       => List(fin)
      case French        => List(fre)
      case German        => List(ger)
      case Greek         => List(gre, grea)
      case Hebrew        => List(heb)
      case Hindi         => List(hin)
      case Icelandic     => List(ice)
      case Indonesian    => List(ins)
      case Italian       => List(ita)
      case Japanese      => List(jap)
      case Javanese      => List(jav)
      case Khmer         => List(khm)
      case Korean        => List(kor)
      case Kurdish       => List(kur)
      case Latin         => List(ita, roma, romm)
      case Mandarin      => List(chi)
      case Malay         => List(mly)
      case Norwegian     => List(nor, dan)
      case Polish        => List(pol)
      case Portuguese    => List(por)
      case Punjabi       => List(pun)
      case Romanian      => List(rmn)
      case Russian       => List(rus)
      case Serbian       => List(ser)
      case Skandinaviska => List(dan, nor, ice, fin, swe)
      case Slovak        => List(slk)
      case Spanish       => List(spa)
      //        object Suryan extends LangName("Suryan") NO CLUE
      case Swahili       => List(swa)
      case Swedish       => List(swe)
      case Tagalog       => List(tag)
      case Tamil         => List(tam)
      case Thai          => List(tha)
      case Turkish       => List(tur, kur)
      case Urdu          => List(urd)
      case Vietnamese    => List(vie)
      case Wu            => List(chi)
      case Xhosa         => List(xho)
      case Zulu          => List(zul)
      case _             => Nil
    }
  }
}

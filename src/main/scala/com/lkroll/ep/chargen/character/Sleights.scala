package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data.{ PsiChiSleights => PsiChiData, PsiGammaSleights => PsiGammaData }

object Sleights {

}

object PsiChiSleights extends Table {
  import PsiChiData._;

  override type Result = PsiSleight;

  val data: RollTable[PsiSleight] = RollTable(
    (1 to 5) -> ambienceSense,
    (6 to 10) -> cognitiveBoost,
    (11 to 15) -> downtime,
    (16 to 19) -> ecoEmpathy,
    (20 to 24) -> emotionControl,
    (25 to 28) -> enhancedCreativity,
    (29 to 32) -> filter,
    (33 to 37) -> grok,
    (38 to 42) -> highPainThreshold,
    (43 to 46) -> hyperthymesia,
    (47 to 51) -> instinct,
    (52 to 56) -> multitasking,
    (57 to 61) -> patternRecognition,
    (62 to 66) -> predictiveBoost,
    (67 to 71) -> qualia,
    (72 to 75) -> savantCalculation,
    (76 to 80) -> sensoryBoost,
    (81 to 85) -> superiorKinesics,
    (86 to 90) -> timeSense,
    (91 to 95) -> unconsciousLead,
    (96 to 100) -> xenoEmpathy);

  override def label: String = "Psi-Chi Sleights";
  override def source: String = "Transhuman p. 45";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
}

object PsiGammaSleights extends Table {
  import PsiGammaData._;

  override type Result = PsiSleight;

  val data: RollTable[PsiSleight] = RollTable(
    (1 to 4) -> alienation,
    (5 to 8) -> aphasicTouch,
    (9 to 12) -> charisma,
    (13 to 16) -> cloudMemory,
    (17 to 21) -> deepScan,
    (22 to 25) -> driveEmotion,
    (26 to 30) -> egoSense,
    (31 to 34) -> empathicScan,
    (35 to 38) -> implantMemory,
    (39 to 42) -> implantSkill,
    (43 to 46) -> mimic,
    (47 to 51) -> mindlink,
    (52 to 56) -> omniAwareness,
    (57 to 60) -> penetration,
    (61 to 65) -> psiShield,
    (66 to 70) -> psychicStab,
    (71 to 74) -> scramble,
    (75 to 78) -> senseBlock,
    (79 to 82) -> senseInfection,
    (83 to 86) -> spam,
    (87 to 90) -> static,
    (91 to 95) -> subliminal,
    (96 to 100) -> thoughtBrowse);

  override def label: String = "Psi-Gamma Sleights";
  override def source: String = "Transhuman p. 45";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
}

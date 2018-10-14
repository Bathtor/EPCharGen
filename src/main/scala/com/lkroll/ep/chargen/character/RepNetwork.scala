package com.lkroll.ep.chargen.character

import com.lkroll.common.macros.Macros
import com.lkroll.ep.chargen._

case class RepNetwork(name: String, networkingField: String) {
  def +(i: Int): CharacterMod.RepMod = CharacterMod.RepMod(Left(this), i);
  def -(i: Int): CharacterMod.RepMod = CharacterMod.RepMod(Left(this), -i);
}
object RepNetwork {
  val circleARep = RepNetwork("@-Rep", "Autonomists");
  val cRep = RepNetwork("c-Rep", "Hypercorps");
  val eRep = RepNetwork("e-Rep", "Ecologists");
  val fRep = RepNetwork("f-Rep", "Media");
  val gRep = RepNetwork("g-Rep", "Criminals");
  val iRep = RepNetwork("i-Rep", "Firewall");
  val rRep = RepNetwork("r-Rep", "Scientists");
  val uRep = RepNetwork("u-Rep", "Ultimates");
  val xRep = RepNetwork("x-Rep", "Gatecrashers");

  val list: List[RepNetwork] = Macros.memberList[RepNetwork];

  def chooseAny(rand: Random, mod: Int): CharacterMod.RepMod = CharacterMod.RepMod(Right(CharacterMod.SkillChoice.PickAny(rand)), mod);
}

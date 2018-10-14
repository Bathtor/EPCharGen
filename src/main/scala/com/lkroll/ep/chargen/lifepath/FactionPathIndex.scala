package com.lkroll.ep.chargen.lifepath

case class FactionPathIndex(i: Int) extends PathIndex {
  override def tableIndex = i;
}

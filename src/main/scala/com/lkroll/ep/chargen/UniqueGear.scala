package com.lkroll.ep.chargen

import com.lkroll.ep.compendium.{Cost, Gear}

object UniqueGear {
  def apply(name: String): Gear = Gear(name, "Unique", "Unique", Cost.None, "Unique", -1)
}

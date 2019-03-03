package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._

object GenderTables {
  val normalGenderTable: RollTable[GenderIdentity] = RollTable(
    (1 to 45) -> GenderIdentity.Male,
    (46 to 90) -> GenderIdentity.Female,
    (91 to 93) -> GenderIdentity.Genderless,
    (94 to 100) -> GenderIdentity.Other);

  val agiGenderTable: RollTable[GenderIdentity] = RollTable(
    (1 to 10) -> GenderIdentity.Male,
    (11 to 20) -> GenderIdentity.Female,
    (21 to 80) -> GenderIdentity.Genderless,
    (81 to 99) -> GenderIdentity.Plurality,
    (100 to 100) -> GenderIdentity.Other);
}

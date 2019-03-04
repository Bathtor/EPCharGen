package com.lkroll.ep.chargen.character

import org.scalatest._
import com.lkroll.ep.compendium.data.AllData.morphModels
import scala.util.Success

class MorphInstantiationTest extends FunSuite with Matchers {

  test("Choice Parser should handle all compendium options") {
    val uniqueOptionsB = Set.newBuilder[String];
    morphModels.flatten.foreach { mm =>
      mm.playerDecisions match {
        case Some(d) => {
          uniqueOptionsB += d;
          val res = ChoiceParser.parseString(d);
          if (res.isFailure) {
            println(s"Matching the following string produced an error: $d");
          }
          res shouldBe a[Success[_]];
        }
        case None => () // ignore
      }
    }
    val uniqueOptions = uniqueOptionsB.result();
    println(s"Unique Player Choices in the Compendium:\n	${uniqueOptions.mkString("\n	")}");
  }

}

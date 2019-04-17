package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler95Test extends FlatSpec with Matchers {
  import Euler95._

  "amicableChain" should "return the amicable chain that we end up in" in {
    amicableChain(220, Seq(220)) should be(Seq(220, 284))
    amicableChain(12496, Seq(12496)) should be(Seq(12496, 14288, 15472, 14536, 14264))
    amicableChain(675, Seq(675)) should be(Seq(6))
  }

}

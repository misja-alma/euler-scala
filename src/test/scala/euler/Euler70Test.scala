package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler70Test extends FlatSpec with Matchers {

  import Euler70._

  "phi" should "return the totient" in {
    phi(2) should be(1)
    phi(3) should be(2)
    phi(4) should be(2)
    phi(9) should be(6)
    phi(132) should be(40)
  }
}

package euler

import org.scalatest.{FlatSpec, Matchers}

import ContinuedFraction._

class ContinuedFractionTest extends FlatSpec with Matchers {

  "getSquareRootContinuedFraction" should "return the correct square root cf" in {
    getSquareRootContinuedFraction(2) should be(ContinuedFraction(2, 1, Seq(2)))
    getSquareRootContinuedFraction(3) should be(ContinuedFraction(3, 1, Seq(1, 2)))
    getSquareRootContinuedFraction(23) should be(ContinuedFraction(23, 4, Seq(1, 3, 1, 8)))
  }
}

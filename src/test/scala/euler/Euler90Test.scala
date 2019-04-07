package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler90Test extends FlatSpec with Matchers {
  import Euler90._

  "permutations" should "return n over m" in {
    permutations(3, 2) should be (Seq(Seq(0, 1), Seq(0, 2), Seq(1, 2)))
    permutations(4, 1) should be (Seq(Seq(0), Seq(1), Seq(2), Seq(3)))
  }

  "nrsHit" should "return the count of numbers hit, exchaning 6's for 9's if needed" in {
    nrsHit(Seq(6, 4), Seq(1, 2, 3, 4)) should be(1)
    nrsHit(Seq(6, 4), Seq(1, 2, 3)) should be(0)
    nrsHit(Seq(6, 4), Seq(4, 6)) should be(2)
    nrsHit(Seq(6, 4), Seq(4, 9)) should be(2)
    nrsHit(Seq(9, 4), Seq(4, 6)) should be(2)
    nrsHit(Seq(9, 6), Seq(9, 6)) should be(2)
  }

  "allSquaresCovered" should "return whether all squares are covered by the 2 cubes" in {
    allSquaresCovered(Seq(0, 5, 6, 7, 8, 9), Seq(1, 2, 3, 4, 8, 9)) should be(true)
    allSquaresCovered(Seq(0, 5, 6, 7, 8, 9), Seq(1, 2, 3, 4, 6, 7)) should be(true)
  }
}

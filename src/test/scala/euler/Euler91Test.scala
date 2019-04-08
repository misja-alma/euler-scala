package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler91Test extends FlatSpec with Matchers {
  import Euler91._

  "rightAngle" should "return true if the vectors are orthogonal" in {
    rightAngle(((0, 0), (1, 0)), ((0, 1), (0, 0))) should be(true)
    rightAngle(((0, 0), (2, 1)), ((2, 1), (1, 3))) should be(true)
    rightAngle(((1, 3), (2, 2)), ((2, 2), (0, 0))) should be(true)
    rightAngle(((0, 0), (2, 1)), ((2, 2), (0, 0))) should be(false)
  }

  "isRightTriangle" should "return true if the triangle has a right angle" in {
    isRightTriangle(Seq((0, 0), (1, 1), (2, 0))) should be(true)
    isRightTriangle(Seq((0, 0), (1, 0), (0, 2))) should be(true)
    isRightTriangle(Seq((0, 0), (2, 2), (1, 3))) should be(true)
    isRightTriangle(Seq((0, 0), (2, 1), (0, 2))) should be(false)
  }

}

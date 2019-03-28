package euler

import org.scalatest.{FlatSpec, Matchers}

import Euler85._

class Euler85Test extends FlatSpec with Matchers {

  "calcRectangleCount" should "calculate the count of rectangles" in {
    calcRectangleCount(1, 1) should be(1)
    calcRectangleCount(2, 1) should be(3)
    calcRectangleCount(3, 1) should be(6)
    calcRectangleCount(4, 1) should be(10)
    calcRectangleCount(3, 3) should be(18)
  }

  "findClosestRectangleCountWithHeight" should "return the rectangle whose count is closest to the threshold" in {
    findClosestRectangleCountWithHeight(1, 1) should be(RectangleResult(1, 1, 1))
    findClosestRectangleCountWithHeight(1, 4) should be(RectangleResult(3, 2, 1))
    findClosestRectangleCountWithHeight(2, 4) should be(RectangleResult(3, 1, 2))
    findClosestRectangleCountWithHeight(2, 18) should be(RectangleResult(18, 3, 2))
  }
}

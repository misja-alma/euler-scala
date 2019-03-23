package euler

import org.scalatest.{FlatSpec, Matchers}
import Euler84._

class Euler84Test extends FlatSpec with Matchers {

  "nextR" should "move to the next railway station" in {
    nextR(atSquare(36)).square should be(5)
    nextR(atSquare(7)).square should be(15)
    nextR(atSquare(22)).square should be(25)
  }

  "nextU" should "move to the next utility station" in {
    nextU(atSquare(36)).square should be(12)
    nextU(atSquare(7)).square should be(12)
    nextU(atSquare(22)).square should be(28)
  }

  def atSquare(square: Int): State = State(square, 0, 0, Vector(), Vector())
}

package euler

import org.scalatest.{FlatSpec, Matchers}

import Euler68._

class Euler68Test extends FlatSpec with Matchers {
  val trips: Array[Array[Int]] = new Array[Array[Int]](5).map(_ => new Array[Int](3))

  "setValue" should "set the correct values in all crossing triplets" in {
    setValue(0, 1)(trips)

    trips(0)(0) should be(1)

    setValue(0, 0)(trips)

    setValue(1, 1)(trips)

    trips(0)(1) should be(1)
    trips(4)(2) should be(1)
    trips(1)(1) should be(0)

    setValue(1, 0)(trips)
    setValue(2, 1)(trips)

    trips(0)(2) should be(1)
    trips(4)(2) should be(0)
    trips(1)(1) should be(1)

    setValue(2, 0)(trips)
    setValue(14, 1)(trips)

    trips(4)(2) should be(1)
    trips(0)(1) should be(1)

    setValue(14, 0)(trips)

    trips.forall(_.forall(_ == 0)) should be (true)
  }

}

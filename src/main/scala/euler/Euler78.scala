package euler

import Utils._

object Euler78 extends App {

  val result = unOrderedPartitions[BigInt].zipWithIndex.find(_._1 % 1000000 == 0).get

  println ("Result: " + result._2)
}

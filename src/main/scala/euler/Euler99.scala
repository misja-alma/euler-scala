package euler

import scala.io.Source

// If we have numbers x and y, y can be written as x ^ xLog (y)
// If we want to see if x^a > y^b, we write first y as a power of x, so y^b = x^ b * xLog(y)
// Then y^b / x^a = x ^ b * xLog(y) / x ^ a = x ^ (b*xLog(y) - a).
// If this is > 1 then y^b is bigger, < 1 then x^a is bigger == 1 means they are equal.
// If a power of a number > 1 that just means that the exponent > 0, so in the end we just want to know if b * xLog(y) - a > 0

object Euler99 extends App {
  val start = System.currentTimeMillis()

  val powerTuples = Source.fromInputStream(getClass.getResourceAsStream("/p099_base_exp.txt"))
    .getLines()
    .map(_.split(",").map(_.toInt))
    .toList

  def comparePowers(power1: Array[Int], power2: Array[Int]): Boolean = {
    val Array(x, px) = power1
    val Array(y, py) = power2
    val xlogy = Math.log(y.toDouble) / Math.log(x.toDouble)
    (py * xlogy - px).signum > 0
  }

  def powerComparator(tuple1: (Array[Int], Int), tuple2: (Array[Int], Int)): Boolean = comparePowers(tuple1._1, tuple2._1)

  val sorted = powerTuples.zipWithIndex.sortWith(powerComparator)
  val (power, index) = sorted.last

  val time = System.currentTimeMillis() - start

  println (s"Solution: ${power.mkString("^")} at line ${index + 1} in $time ms")
}

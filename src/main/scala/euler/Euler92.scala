package euler

import Utils._

object Euler92 extends App {
  // Just iterate until a know nr is found and hold all calculated chain ends in an array.

  val ends = new Array[Int](10000000)

  def square(x: Int): Int = x * x

  def endOfSquareDigitChain(start: Int): Int = {
    if (ends(start) == 0) {
      ends(start) = if (start == 1 || start == 89) start else {
        val nextStart = digits(start).map(square).sum
        endOfSquareDigitChain(nextStart)
      }
    }
    ends(start)
  }

  val solution = (1 until 10000000).count(endOfSquareDigitChain(_) == 89)

  println ("Solution: " + solution)
}

package euler

import ContinuedFraction._

object Euler80 extends App {

  def digitalSum(x: BigDecimal): Int = {
    x.toString().filterNot(_ == '.').take(100).map(_.asDigit).sum
  }

  val solution = (1 to 100)
    .map(n => approximateSqrt(n, 1000))
    .filterNot(_.isWhole)
    .map(digitalSum)
    .sum

  println ("Solution: " + solution)
}

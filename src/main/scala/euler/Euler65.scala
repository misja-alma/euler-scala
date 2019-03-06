package euler

import ContinuedFraction._

object Euler65 extends App {
  // e series index:
  // n == 0 -> 2
  // n > 0  -> m = (n-1)/3, r = (n-1) % 3
  //     r = 0 or 2 -> result = 1
  //     r = 1 -> 2 * (m + 1)

  def eIndex(n: Int): Int = if (n == 0) 2 else {
    if ((n - 1) % 3 == 1) {
      val k = (n - 1) / 3
      2 * (k + 1)
    } else {
      1
    }
  }
  
  def eSeries(length: Int) = (0 until length) map eIndex

  println (Math.E)
  val res = evaluateCfSeriesAsFraction(eSeries(100000).reverse)
  println (res)
  println (BigDecimal(res._1) / BigDecimal(res._2))

  println("Solution: " + Utils.digits(res._1).sum)
}

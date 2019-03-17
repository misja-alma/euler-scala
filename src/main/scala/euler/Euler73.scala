package euler

import euler.Utils._

object Euler73 extends App {
  // For each denominator starting at 4; start at the nearest fraction below 1/3 and keep increasing the numerator until above/ equal 1/2.
  // The nearest numerator yielding a fraction below 1/3 is the denominator divided by 3 rounded down.
  // Everything that falls into the interval is a candidate. This seems doable since we only have to check for d <= 12000

  val solution = (4 to 12000).map { d => {
    val start = d / 3 + 1
    val end = d / 2 - (if (d % 2 == 0) 1 else 0)
    (start to end).count { n => {
      gcd(n, d) == 1
    }}
  }}.sum

  println ("Solution: " + solution)
}

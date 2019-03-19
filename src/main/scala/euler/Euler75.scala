package euler

import Utils._

object Euler75 extends App {
  // For Pythagorean triples, the following holds:
  // a^2 + b^2 = c^2  =>
  // There exist integers n and m such that:
  // a = m^2 - n^2
  // b = 2mn
  // c = m^2 + n ^2
  // (note that m has to be greater than n and both should be greater than zero)
  // Note: this is Euclid's algo and it produces every primitive triple (if m and n coprime and one of them even), but not every triple!
  // The length of the corresponding triangle, a + b + c, can thus be rewritten as: L = 2m^2 + 2mn
  // So for this problem, we want to find all L's <= 1500000 that match this formula, and that match this formula exactly once.
  // So, enumerate all n's and m's and keep the resulting L's in a frequency map; the solution is the nr of keys with frequency one.
  // problem: this list is not exhaustive! Because multiples of solutions are also solutions: but multiplying a, b or c yields a m or n that is the -root- of the multiple bigger!
  // This is logical because it says nowhere that n and m should be whole numbers.
  // So: start with all primtives. Then add all multiples of every triple. Count the ones that have unique length.s


  // For each n: take m = n + 1 until m => L > 1500000
  // Stop when L(n+1) is > 1500000

  val maxLength = 1500000L

  def calcLength(m: Int, n: Int): Long = {
    2L * m * m + 2L * m * n
  }

  val allLengths = Stream
    .from(1)
    .takeWhile(n => calcLength(n + 1, n) <= maxLength)
    .flatMap(n => {

      Stream.from(start = n + 1, step = 2)  // one of n and m should be odd
        .filter(m => gcd(m, n) == 1) // and they should be coprime. This will give all primitives
        .flatMap(m => {
          val firstLength = calcLength(m, n)
          if (firstLength > maxLength) Seq(firstLength) else {  // be sure to add one (too large) entry so the stream can terminate
            firstLength to maxLength by firstLength  // add all multiples of primitives
          }
        })
        .takeWhile(_ <= maxLength)
    })
  
  val solution = allLengths.groupBy(identity).count(_._2.size == 1)

  println("Solution: " + solution)
}

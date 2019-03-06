package euler

import ContinuedFraction._

object Euler66 extends App {
   // The problem is a Pell equation. This can be solved using the continued fraction of sqrt(D)
   // See p 261..263 of Recreations in the theory of numbers.
   // Note: It seems we have to search through the cf series until we find Q == 1. This always happens at the end of
   // a period, however, it can also happen earlier. I think we should try to find the earliest match? 
   // And also we have to adjust the calc. cf function so that it returns or finds Q's, right now it only gives a's.
   // -> Note that we need to find a value of Q == 1 for an odd value of n, and that it is the p and q of (n-1) which will be used as solution.
  // -> can be done even more simple: we just go through the series of p+q's until we find a matching solution!
  // Btw, pn and qn are the numerator and denominator of the nth convergent (of D) as a fraction

  def findSmallestSolution(d: Int): (BigInt, BigInt) = {
    // Requires that d is not a square
    val cf = getSquareRootContinuedFraction(d)
    // Turn into series (ignore whole part) of p q
    // find first (p, q) for which p^2 -d*q^2 == 1
    val series: Stream[Int] = Stream.continually(cf.repeatingSequence).flatten

    Stream.from(1).map(n => series.take(n).toList).map(s =>
      evaluateCfSeriesAsFraction((cf.a0 +: s).reverse)
    ).find(solvesPell(d)).get
  }

  def solvesPell(d: Int)(pq: (BigInt, BigInt)): Boolean =  {
    val (p, q) = pq
    p * p - d * q * q == 1
  }

  val solution = (1 to 1000).filterNot(isSquare).map(d => (d, findSmallestSolution(d))).maxBy(_._2._1)

  println ("Solution: " + solution)

  def isSquare(x: Int) = {
    val s = Math.sqrt(x).toInt
    s * s == x
  }

}

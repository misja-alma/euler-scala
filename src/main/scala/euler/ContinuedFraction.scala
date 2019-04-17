package euler

import scala.annotation.tailrec

/**
  * @param n  In case of a sqrt, the nr the sqrt was taken of
  * @param a0 The whole part of the fraction
  * @param repeatingSequence The sequence representing the fraction
  */
case class ContinuedFraction(n: Int, a0: Int, repeatingSequence: Seq[Int]) {
  override def toString: String = s"Sqrt($n)=[$a0;(${repeatingSequence.mkString(",")})], period=$period"

  def period: Int = repeatingSequence.size

  import ContinuedFraction._

  // Take the last el. from the series, take its reciprocal: this becomes b in el(-1) + b; if there are elements left, then take reciprocal and continue
  def evaluate(length: Int): Double = {
    if (period == 0) a0 else {
      val repeatingSeries = Seq.fill(length / period + 1)(repeatingSequence).flatten
      val series = (Seq(a0) ++ repeatingSeries.take(length - 1)).reverse
      evaluateCfSeries(series)
    }
  }
}

object ContinuedFraction {
  /**
    * Note: the cf series should be the complete series, i.e. including a0
    */
  @tailrec
  def evaluateCfSeries(reverseCfSeries: Seq[Int], b: Double = Double.PositiveInfinity): Double =
    if (reverseCfSeries.isEmpty) b else {
      val newB = reverseCfSeries.head + 1/b
      evaluateCfSeries(reverseCfSeries.tail, newB)
    }

  /**
    * Note: the cf series should be the complete series, i.e. including a0
    */
  @tailrec
  def evaluateCfSeriesAsFraction(reverseCfSeries: Seq[Int], b: (BigInt, BigInt) = (1, 0)): (BigInt, BigInt) =
    if (reverseCfSeries.isEmpty) b else {
      val reciprocal = b.swap
      val newB = (reciprocal._1 + reverseCfSeries.head * reciprocal._2, reciprocal._2)
      evaluateCfSeriesAsFraction(reverseCfSeries.tail, newB)
    }

  // precision is the length of the cf series that we use: a rough guess is that it should be 4 times the nr of decimal digits precision required.
  // NOTE -> Newton approximation converges much faster!
  def approximateSqrt(x: Int, precision: Int): BigDecimal = {
    val cf = getSquareRootContinuedFraction(x)
    if (cf.repeatingSequence.isEmpty) cf.a0 else {
      val series: Stream[Int] = Stream.continually(cf.repeatingSequence).flatten
      val fraction = evaluateCfSeriesAsFraction((cf.a0 +: series.take(precision)).reverse)
      BigDecimal(fraction._1) / BigDecimal(fraction._2)
    }
  }

  // a0: sqrt(n).toInt
  // a1:
  // x1 = (a(sqrt(n)) + b) / c   For iteration 1, a = 1, b = a0, c = x - (a0)^2
  // If c == 1 then stop; iterations is iterations so far + 1
  // an: replace sqrt(n) with a0 in xn and round the expression downwards.
  // xn+1: c(a(sqrt(x)) - p)/ (a^2x - p^2) where p = b - an - c
  // from this we can can calculate new a, b and c for the next iteration

  // TODO we could maybe use an Iterator here.
  def getSquareRootContinuedFraction(n: Int): ContinuedFraction = {
    val root = Math.sqrt(n.toDouble)
    val a0 = root.toLong
    if (a0 * a0 == n) {
      ContinuedFraction(n.toInt, a0.toInt, Seq())
    } else {
      var b = a0
      var c = n - a0*a0
      var nextA0 = a0
      var cfs = Seq[Int]()
      while (cfs.isEmpty || (!(b == a0 && c == 1))) {    // Continue until we have no denominator and the counter is the sqrt + a0 again.
        nextA0 = (a0 + b) / c
        cfs = cfs :+ nextA0.toInt
        val p = b - nextA0 *  c
        b = -p
        c = (n - p * p) / c    // For some reason this division never seems to have a remainder ..
      }
      // Special case: the last a0 could be equal to the previous one and there was only 1 iteration. In that case, the period is not 2 but 1.
      val lastA0 = a0 + b
      if (!(lastA0 == nextA0 && cfs.size == 1)) {
        cfs = cfs :+ lastA0.toInt
      }
      ContinuedFraction(n.toInt, a0.toInt, cfs)
    }
  }
}

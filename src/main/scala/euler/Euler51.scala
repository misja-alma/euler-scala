package euler

import Utils._

object Euler51 extends App {
  // Note that to prevent division by 3, we need to have 3 same digits (or 6) so that changing them won't change the modulo 3 digit sum. Also the last digit can't be included
  // because it can't be even or 5.
  // For each prime: check if it has at least 3 same digits (not including the last one). If so, then for every higher combination of 3, check if at least 7 other prime exists which are
  // the same except for the 3 digits which should be 3 other ones (beware of leading 0's). Note that since 7 should be higher, we are only interested in 0's, 1's or 2's. Return the first one found.

  implicit val primeCache = new PrimeCache[Long]()

  // Returns all combinations of indices for which x's digit is the same 3 times
  def get3DigitCombinations(x: Seq[Int]): Seq[Seq[Int]] =
    x.indices.combinations(3).filter(_.map(x).toSet.size == 1).toSeq

  def primesSharingDigits(primeDigits: Seq[Int], sharedIndices: Seq[Int]): Int = {
    val start = if (sharedIndices.head == 0) 1 else 0
    (start to 9).count { digitValue =>
      val primeAr = primeDigits.toArray
      sharedIndices.foreach(di => primeAr(di) = digitValue)
      isPrime(fromDigits(primeAr))
    }
  }

  def isEightPrimeFamily(prime: Long): Boolean = {
    val asDigits = digits(prime)

    get3DigitCombinations(asDigits)
      .filterNot(_.last == asDigits.length - 1)
      .filterNot(ds => asDigits(ds.head) > 2)
      .exists(ds => primesSharingDigits(asDigits, ds) >= 8)
  }

  val Some(result) = primes.find(isEightPrimeFamily)

  println("Solution: " + result)
}

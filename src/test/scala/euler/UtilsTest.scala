package euler

import euler.Utils._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class UtilsTest extends FlatSpec with Matchers {

  "isPrime" should "correctly recognize prime numbers" in {
    implicit val cache = new PrimeCache[Long]

    isPrime(2) should be(true)
    isPrime(1) should be(false)
    isPrime(4) should be(false)
    isPrime(25) should be(false)
    isPrime(11) should be(true)
    isPrime(12) should be(false)
    isPrime(21) should be(false)
    isPrime(37) should be(true)
  }

  "primes" should "return a stream of primes" in {
    implicit val cache = new PrimeCache[Long]

    primes.take(5).toList should be(List(2, 3, 5, 7, 11))
  }

  "toLong" should "return the digits as a long" in {
    toLong(Seq(1, 2, 3)) should be(123L)
    toLong(Seq(1)) should be(1L)
  }

  "pairs" should "return all possible pairs" in {
    pairs(Set(1, 2, 3)) should be(Set((1, 2), (1, 3), (2, 3)))
  }

  "digits" should "return the seq of digits in the number" in {
    digits(123) should be(Seq(1, 2, 3))
    digits(0) should be(Seq(0))
    digits(1122) should be(Seq(1, 1, 2, 2))

    digits(13L, 8L) should be(Seq(1, 5))
    digits(BigInt(128), BigInt(2)) should be(Seq(1, 0, 0, 0, 0, 0, 0, 0))
  }

  "concat" should "return the concatenation of 2 ints" in {
    concat(12, 34) should be(1234L)
    concat(1, 1) should be(11L)
    concat(10, 100) should be(10100L)
  }

  "fac" should "return the factorial" in {
    fac(0) should be(1)
    fac(1) should be(1)
    fac(2) should be(2)
    fac(6) should be(720)
  }

  "gcd" should "return the greatest common divisor" in {
    gcd(6, 4) should be(2)
    gcd(6, 2) should be(2)
    gcd(BigInt(6), BigInt(1)) should be(BigInt(1))
    gcd(6, 5) should be(1)
    gcd(128, 4) should be(4)
    gcd(12, 9) should be(3)
    gcd(13, 5) should be(1)
    gcd(-6, 2) should be(2)
  }

  "distinctPrimeFactors" should "return all distinct prime factors" in {
    implicit val factorCache = collection.mutable.Map[Long, Seq[Long]]()
    implicit val primeCache: Utils.PrimeCache[Long] = new PrimeCache[Long]

    primeFactors(2) should be(Seq(2))
    primeFactors(3) should be(Seq(3))
    primeFactors(4) should be(Seq(2, 2))
    primeFactors(6) should be(Seq(2, 3))
    primeFactors(24) should be(Seq(2, 2, 2, 3))
    primeFactors(25) should be(Seq(5, 5))
  }

  "isPermutation" should "return true if both arguments are permutations" in {
    isPermutation(12, 21) should be(true)
    isPermutation(13, 13) should be(true)
    isPermutation(111, 111) should be(true)
    isPermutation(0, 0) should be(true)
    isPermutation(1, 1) should be(true)
    isPermutation(9, 9) should be(true)
    isPermutation(9997, 7999) should be(true)
    isPermutation(91, 19) should be(true)
    isPermutation(999999991, 199999999) should be(true)
    isPermutation(199999999, 999999991) should be(true)

    isPermutation(0, 1) should be(false)
    isPermutation(1, 10) should be(false)
    isPermutation(1, 11) should be(false)
    isPermutation(9997, 7779) should be(false)
  }

  "unOrderedPartitions" should "return the stream of the nr of unordered partitions" in {
    unOrderedPartitions[Long].take(6).toList should be(List(1, 1, 2, 3, 5, 7))
  }

  "combinationSeqs" should "return the set of all sequences that can be created by combining elements" in {
    val multiply: (Long, Long) => Long = Numeric.LongIsIntegral.times

    combinationSeqs[Long](ArrayBuffer(1, 2, 3), multiply).map(_.sorted) should be(Set(Seq(1, 2, 3), Seq(2, 3), Seq(1, 6), Seq(2, 3), Seq(6)))
    combinationSeqs[Long](ArrayBuffer(2, 2, 3), multiply).map(_.sorted) should be(Set(Seq(2, 2, 3), Seq(3, 4), Seq(2, 6), Seq(12)))

    def pruneWithSumAbove7(ar: ArrayBuffer[Long]): Boolean = ar.sum > 7
    combinationSeqs[Long](ArrayBuffer(2, 2, 3), multiply, pruneWithSumAbove7).map(_.sorted) should be(Set(Seq(2, 2, 3), Seq(3, 4)))
  }
}

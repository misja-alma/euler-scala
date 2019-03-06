package euler

import euler.Utils.PrimeCache
import org.scalatest.{FlatSpec, Matchers}

class UtilsTest extends FlatSpec with Matchers {

  "isPrime" should "correctly recognize prime numbers" in {
    implicit val cache = new PrimeCache[Long](2, 3)

    Utils.isPrime(2) should be(true)
    Utils.isPrime(1) should be(false)
    Utils.isPrime(4) should be(false)
    Utils.isPrime(25) should be(false)
    Utils.isPrime(11) should be(true)
    Utils.isPrime(12) should be(false)
    Utils.isPrime(21) should be(false)
    Utils.isPrime(37) should be(true)
  }

  "primes" should "return a stream of primes" in {
    implicit val cache = new PrimeCache[Long](2, 3)

    Utils.primes.take(5).toList should be(List(2, 3, 5, 7, 11))
  }

  "toLong" should "return the digits as a long" in {
    Utils.toLong(Seq(1, 2, 3)) should be(123L)
    Utils.toLong(Seq(1)) should be(1L)
  }

  "pairs" should "return all possible pairs" in {
    Utils.pairs(Set(1, 2, 3)) should be(Set((1, 2), (1, 3), (2, 3)))
  }

  "digits" should "return the seq of digits in the number" in {
    Utils.digits(123) should be(Seq(1, 2, 3))
    Utils.digits(0) should be(Seq(0))
    Utils.digits(1122) should be(Seq(1, 1, 2, 2))
  }

  "concat" should "return the concatenation of 2 ints" in {
    Utils.concat(12, 34) should be(1234L)
    Utils.concat(1, 1) should be(11L)
    Utils.concat(10, 100) should be(10100L)
  }
}

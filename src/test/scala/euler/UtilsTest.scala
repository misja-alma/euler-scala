package euler

import euler.Utils._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

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
  }

  "concat" should "return the concatenation of 2 ints" in {
    concat(12, 34) should be(1234L)
    concat(1, 1) should be(11L)
    concat(10, 100) should be(10100L)
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

  "primeFactors" should "return all distinct prime factors" in {
    implicit val factorCache: mutable.Map[Long, Set[Long]] = collection.mutable.Map[Long, Set[Long]]()
    implicit val primeCache: Utils.PrimeCache[Long] = new PrimeCache[Long]

    primeFactors(2) should be(Set(2))
    primeFactors(3) should be(Set(3))
    primeFactors(4) should be(Set(2))
    primeFactors(6) should be(Set(2, 3))
    primeFactors(24) should be(Set(2, 3))
    primeFactors(25) should be(Set(5))
  }
}

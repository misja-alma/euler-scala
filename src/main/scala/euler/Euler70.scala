package euler

import scala.collection.mutable
import Utils._

object Euler70 extends App {
  // phi(n) = n * product(1 - 1/p) for all distinct prime nrs that divide n (including n itself when it is a prime)
  // For each n: factor n by dividing by all primes <= sqrt(n). If a divisor is found then the factors are the union set
  // of both found divisors; or phi can be calculated directly from both earlier phi's and their gcd.
  // or better: keep the product(1 - 1/p) for every checked nr.
  // Check if phi is a permutation of n (maybe add some optimizations) and if so, keep it together with n for later use.

  implicit lazy val factorCache: mutable.Map[Long, Set[Long]] = collection.mutable.Map[Long, Set[Long]]()
  implicit lazy val primeCache: Utils.PrimeCache[Long] = new Utils.PrimeCache[Long]

  def phi(n: Int): Int = (Fraction(n.toLong) * halfPhi(primeFactors(n))).toIntegral.toInt

  // we could cache the halfPhi's as well ..
  def halfPhi(primeFactors: Set[Long]): Fraction[Long] = primeFactors.map(p => Fraction(p - 1, p)).product

  val (bestN, bestPhi) = (2 until 10000000)
    .map(n => (n, phi(n)))
    .filter(nphi => isPermutation(nphi._1, nphi._2))
    .map{nphi => println(nphi); nphi }
    .minBy(nphi => nphi._1.toDouble / nphi._2)

  println ("Solution: " + bestN)
}

package euler

import euler.Utils.{FactorCache, PrimeCache, primeFactors}

import scala.collection.immutable.Set

object Totient {

  def phi(n: Int)(implicit factorCache: FactorCache[Long], primeCache: PrimeCache[Long]): Int =
    (Fraction(n.toLong) * halfPhi(primeFactors(n).toSet)).toIntegral.toInt

  // we could cache the halfPhi's as well ..
  def halfPhi(primeFactors: Set[Long]): Fraction[Long] = primeFactors.map(p => Fraction(p - 1, p)).product
}

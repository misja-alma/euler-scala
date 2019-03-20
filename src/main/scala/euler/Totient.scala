package euler

import euler.Utils.{FactorCache, PrimeCache, distinctPrimeFactors}

import scala.collection.immutable.Set

object Totient {

  def phi(n: Int)(implicit factorCache: FactorCache[Long], primeCache: PrimeCache[Long]): Int = (Fraction(n.toLong) * halfPhi(distinctPrimeFactors(n))).toIntegral.toInt

  // we could cache the halfPhi's as well ..
  def halfPhi(primeFactors: Set[Long]): Fraction[Long] = primeFactors.map(p => Fraction(p - 1, p)).product
}

package euler

import Totient._

object Euler72 extends App {
  // We want to count all combinations n,d where n < d and d <= 1000000, and n and d are relative primes.
  // That seems to be the sum of all phi's for d (2 .. 1000000)

  implicit lazy val factorCache: Utils.FactorCache[Long] = collection.mutable.Map[Long, Seq[Long]]()
  implicit lazy val primeCache: Utils.PrimeCache[Long] = new Utils.PrimeCache[Long]

  val solution = (2 to 1000000).map(phi).map(_.toLong).sum

  println ("Solution: " + solution)
}

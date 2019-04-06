package euler

import euler.Utils._

import scala.collection.mutable.ArrayBuffer

object Euler88 extends App {
  // For each n; try a sum S of n+1 or bigger until matching product is found
  // To find the product: Factor S in prime factors. (In case of primes this should return the prime, otherwise 1 and S should be excluded)
  // Take the power set of the prime factors set. (Use set.subsets.toSet)
  // For each el of the power set, calc its sum + n - set.size and find the one which equals S.
  // If none found, increase S, etc.
  // If this is too slow then a smarter search of the power sets is needed. But should be fast since we're only searching until 12000 so max 14 prime factors or so.

  // Note: found productSums can be reused! Because they can easily be changed to a sum that is 1 lower by splitting a 4 into 2 2's; this compensates for a new value of k that requires 1 extra 1.
  // Could it be that the set of possible product sum nr's is limited?
  // Also, for each k, there is the product sum nr 2 * k: This is the sequence, k, 2, and (k - 2 times 1). It can only be that there is another smaller candidate.

  implicit lazy val factorCache: FactorCache[Long] = collection.mutable.Map[Long, Seq[Long]]()
  implicit lazy val primeCache: PrimeCache[Long] = new PrimeCache[Long]

  def hasProductSum(k: Int)(sum: Int): Boolean = {
    val pruneFilter: ArrayBuffer[Long] => Boolean = pf => pf.sum + k - pf.size > sum

    val pfCombinations = combinationSeqs(ArrayBuffer(primeFactors(sum): _*), Numeric.LongIsIntegral.times, pruneFilter)
    pfCombinations.exists(pf => pf.sum + k - pf.size == sum)
  }

  def lowestProductSum(k: Int): Int =
    Stream.from(k + 1).find(hasProductSum(k)).get

  val allNrs = (2 to 12000).map(lowestProductSum)

  println ("Solution: " + allNrs.toSet.sum)    // Answer: 7587457. In 55 sec ...
}

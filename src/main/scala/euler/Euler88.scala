package euler

import scala.collection.mutable

object Euler88 extends App {
  // For each k: start with only 1's. If not a product sum, add new (distinct) candidates that differ 1 in sum to a pq
  // Take top from pq, check if product sum, etc. When a product sum is found, stop and take next k. Otherwise add new candidates.
  // Don't add new candidates when the product > the sum.
  // Add each found product sum to a set.
  // Selecting new candidates: For this we need to have the set of nr's grouped. We want to increase only 1 of each group.
  // So: best representation is actually the frequency count (plus the total sum and product) instead of a long list of nr's.
  // TODO We could for instance skip candidates (where sum != product) where the highest nr, when increased with 1, already exceeds the product.
  // We could also skip all candidates where all nr's are 1 except 1. But this doesn't make much of a difference.

  // Another way; for each n; try a sum S of n+1 or bigger until matching product is found
  // To find the product: Factor S in prime factors. (In case of primes this should return the prime, otherwise 1 and S should be excluded)
  // Take the power set of the prime factors set. (Use set.subsets.toSet)
  // For each el of the power set, calc its sum + n - set.size and find the one which equals S.
  // If none found, increase S, etc.
  // If this is too slow then a smarter search of the power sets is needed. But should be fast since we're only searching until 12000 so max 14 prime factors or so.



  case class ProductSumCandidate(counts: Map[Int, Int], sum: Int, product: Int, highestNr: Int) {
    override def hashCode(): Int = counts.hashCode()

    override def equals(obj: Any): Boolean = {
      if (!obj.isInstanceOf[ProductSumCandidate]) false else {
        val other = obj.asInstanceOf[ProductSumCandidate]
        counts.equals(other.counts)
      }
    }
  }

  implicit val productSumCandidateOrdering = new Ordering[ProductSumCandidate] {
    // A lower sum is better. With equal sums, prefer the lower diff between sum and product (?)
    override def compare(x: ProductSumCandidate, y: ProductSumCandidate): Int = {
      val result = y.sum.compareTo(x.sum)
      if (result == 0) {
        Math.abs(y.sum - y.product).compareTo(Math.abs(x.sum - x.product))
      } else {
        result
      }
    }
  }

  def generateNewCandidates(candidate: ProductSumCandidate): Seq[ProductSumCandidate] =
    candidate.counts
      .filterNot(_._2 == 0)
      .map { case (nr, count) => {
      val newNr = nr + 1
      val existingCount = candidate.counts.getOrElse(newNr, 0)
      val newCounts = candidate.counts + (nr -> (count - 1)) + (newNr -> (existingCount + 1))
      val newSum = candidate.sum + 1
      val newProduct = candidate.product / nr * newNr
      ProductSumCandidate(newCounts, newSum, newProduct, Math.max(newNr, candidate.highestNr))
    }}
      .filterNot(c => c.product > c.sum || (c.product != c.sum) && (c.product / c.highestNr * (c.highestNr + 1) > c.sum))
      .toSeq

  def lowestProductSum(k: Int): Int = {
    val pq = new mutable.PriorityQueue[ProductSumCandidate]()
    pq += ProductSumCandidate(Map(1 -> k), k, 1, 1)
    val tried = collection.mutable.Set[ProductSumCandidate]()

    var result: Option[Int] = None

    while (result.isEmpty && pq.nonEmpty) {
      val next = pq.dequeue()
      if (next.sum == next.product) {
        result = Some(next.sum)
      } else {
        val newCandidates = generateNewCandidates(next).filterNot(tried)
        tried ++= newCandidates
        pq.enqueue(newCandidates: _*)
      }
    }

    // Check: can there ever be no result for some value of k?
    println (s"k: $k sum: ${result.get}")
    result.get
  }

  val allNrs = (2 to 12000).map(lowestProductSum)

  println ("Solution: " + allNrs.toSet.sum)    // 7587457
}

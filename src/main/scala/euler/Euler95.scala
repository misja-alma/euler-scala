package euler

import Utils._

import scala.annotation.tailrec

object Euler95 extends App {
  lazy val limit = 1000000

  lazy implicit val primeCache = new PrimeCache[Long]()
  lazy implicit val divisorCache = collection.mutable.Map[Long, Set[Long]]()

  @tailrec
  def amicableChain(i: Long, alreadyChecked: Set[Long] = Set(), chainSoFar: Seq[Long] = Seq()): Seq[Long] = {
    if (alreadyChecked.contains(i)) {
      chainSoFar.reverse.dropWhile(_ != i)
    } else {
      if (i < 1 || i > limit) Seq() else {
        val next = divisors(i).sum
        amicableChain(next, alreadyChecked + i, i +: chainSoFar)
      }
    }
  }

  val bestChain = (1 to limit)
    .map(_.toLong)
    .map(i => amicableChain(i))
    .maxBy(_.length)

  println(s"Solution: ${bestChain.min} from chain: $bestChain")
}

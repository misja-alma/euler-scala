package euler

import Utils._

import scala.annotation.tailrec

object Euler95 extends App {
  lazy val limit = 1000000

  lazy implicit val primeCache = new PrimeCache[Long]()
  lazy implicit val divisorCache = collection.mutable.Map[Long, Set[Long]]()

  @tailrec
  def amicableChain(i: Long, alreadyChecked: Seq[Long]): Seq[Long] = {
    val next = divisors(i).sum
    if (next < 1 || next > limit) Seq() else {
      if (alreadyChecked.contains(next)) {
        alreadyChecked.reverse.dropWhile(_ != next)
      } else {
        amicableChain(next, next +: alreadyChecked)
      }
    }
  }

  val bestChain = (1 to limit)
    .map(_.toLong)
    .map(i => amicableChain(i, Seq(i)))
    .maxBy(_.length)

  println (s"Solution: ${bestChain.min} from chain: $bestChain")
}

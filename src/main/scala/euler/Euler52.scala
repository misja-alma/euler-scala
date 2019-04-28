package euler

import Utils._

object Euler52 extends App {

  def isPermutedMultiple(nrMultiples: Int)(x: Int): Boolean = {
    val ds = digits(x).toSet
    (2 to nrMultiples).forall { m => digits(x * m).toSet == ds}
  }

  val start = System.currentTimeMillis()

  val Some(solution) = Stream.from(1).find ( isPermutedMultiple(6) )

  val time = System.currentTimeMillis() - start

  println (s"Solution: $solution in $time ms")
}

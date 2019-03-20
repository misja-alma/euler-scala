package euler

import Utils._

object Euler76 extends App {
  // Just using dynamic programming is too slow because of all the paths that have to be carefully combined; we don't want to count doubles.
  // So used a recurrence relation for the partition function.

  val solution = unOrderedPartitions[Long].take(101).last - 1

  println("Solution: " + solution)
}

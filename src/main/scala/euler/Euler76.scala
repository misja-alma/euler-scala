package euler

object Euler76 extends App {
  // Just using dynamic programming is too slow because of all the paths that have to be carefully combined; we don't want to count doubles.
  // So used a recurrence relation for the partition function.

  val paths = new Array[Long](101)
  paths(0) = 1
  def path(i: Long): Long = if (i < 0) 0 else paths(i.toInt)

  // p(n) = sum(k>1) -1^(k+1) * (p(n - k(3k - 1)/2) + p(n - k(3k + 1)/2))
  def generator(n: Int): Long = {

    def previousValues(k: Int): Long = {
      val i1 = n.toLong - (3L * k - 1) * k / 2
      val i2 = n.toLong - (3L * k + 1) * k / 2
      path(i1) + path(i2)
    }

    Stream
      .from(1)
      .map(previousValues)
      .takeWhile(_ > 0)
      .foldLeft((1, 0L)) { case ((k, acc), pr) => (k * -1, acc + k * pr) }
      ._2
  }

  (1 to 100).foreach(n => paths(n) = generator(n))

  println("Solution: " + (paths(100) - 1))
}

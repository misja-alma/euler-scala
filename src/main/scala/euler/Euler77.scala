package euler

import Utils._

object Euler77 extends App {
  // paths(n) is the union of all paths(a) and paths(b) where a + b = n
  // So have an array of sets of paths and build until size > 5000. Sort the individual paths to be able to compare them.

  implicit val cache = new PrimeCache[Long]

  val paths = new Array[Set[List[Int]]](101)

  def combinePaths(paths1: Set[List[Int]], paths2: Set[List[Int]]): Set[List[Int]] =
    paths1.flatMap(p1 => paths2.map { p2 =>
      (p1 ++ p2).sorted
    })

  def addPaths(n: Int): Unit = {
    val ps = primes.takeWhile(_ <= n/2).flatMap(term => combinePaths(paths(term.toInt), paths(n - term.toInt)))
    if (isPrime(n)) paths(n) = ps.toSet + List(n) else paths(n) = ps.toSet
  }

  val solution = Stream.from(1).takeWhile(n => {
    addPaths(n)
    paths(n).size - 1 <= 5000
  }).last + 1

  println("Solution: " + solution)
}

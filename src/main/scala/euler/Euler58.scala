package euler

import Utils._

object Euler58 extends App {

  case class Stats(iteration: Int, n: Int, primes: Int) {
    def primeRatio: Double = primes.toDouble / n

    def sideLength: Int = 2 * iteration + 1
  }

  implicit val primeCache = new PrimeCache[Long]

  // Start iterating with only the number '1', so 0 primes and 1 number (iteration 0)
  val diagonals = Iterator.iterate(Stats(0, 1, 0)) { case Stats(oldI, n, p) => {
    val i = oldI + 1
    val newNumbers = Seq(square(2 * i + 1), square(2* i) + 1, square(2 * i) - (2 * i - 1), square(2 * i + 1) - 2 * i)
    println (s"Numbers for iteration $i: $newNumbers")                     
    val primes = newNumbers.count(x => isPrime(x))
    val result = Stats(i, n + 4, p + primes)
    println(s"New primes: $primes, prime ratio: ${result.primeRatio}, side length: ${result.sideLength}")
    result
  }}

  val firstSuccess = diagonals.drop(1).dropWhile(_.primeRatio >= 0.1).next()

  println ("Result: " + firstSuccess.sideLength)
}

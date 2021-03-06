package euler

import Utils._

object Euler87 extends App {
  // For each prime where 4th power < 50000000; try 2nd, 3rd, 4th power; for each power:
  // again for each prime >= p1 where power3 is smaller than 50000000 - the result of the power for p1; for each remaining power;
  // again for each prime >= p2 where the power is smaller than 50000000 - the sum of the p1 and p2 result; for the remaining power;
  // add the sum of p1, p2, p3 to a Set.

  val threshold = 50000000

  implicit val cache = new PrimeCache[Long]()

  def pow(x: Long, p: Int): Long = if (p == 0) 1 else x * pow(x, p - 1)

  val all = for {
    p1 <- primes.takeWhile(p => pow(p, 4) < threshold)
    power1 <- 2 to 4
    pr1 = pow(p1, power1)

    p2 <- primes.dropWhile(_ < p1).takeWhile(p => pow(p, 3) < threshold - pr1)
    power2 <- (2 to 4).filterNot(_ == power1)
    pr2 = pow(p2, power2)

    power3 <- (2 to 4).filterNot(p => p == power1 || p == power2)
    p3 <- primes.dropWhile(_ < p2).takeWhile(p => pow(p, power3) < threshold - pr1 - pr2)
    pr3 = pow(p3, power3)
  } yield {
     pr1 + pr2 + pr3
  }

  println ("Solution: " + all.toSet.size)
}

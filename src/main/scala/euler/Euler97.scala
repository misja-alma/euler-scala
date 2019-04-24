package euler

object Euler97 extends App {

  val solution = (BigInt(2).pow(7830457) * 28433 + 1) % 10000000000L

  println ("Solution: " + solution)
}

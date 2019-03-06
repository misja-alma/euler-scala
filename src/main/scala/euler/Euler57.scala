package euler

import Utils._

object Euler57 extends App {

  val sqc = Iterator.iterate((BigInt(3), BigInt(2))) { case (t, n) => println (s"$t / $n"); (2 * n + t, n + t )}

  val result = sqc.take(1000).count { case (t, n) => digits(t).length > digits(n).length }

  println ("Result: " + result)
}

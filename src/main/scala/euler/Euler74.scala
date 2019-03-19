package euler

import Utils._

import scala.annotation.tailrec

object Euler74 extends App {
  // For each nr, count all els in the series until a repeater is found

  val solution = (1 until 1000000).map(factorialSeries).count(_.size == 60)

  def factorialSeries(n: Int): Set[Int] = {
    @tailrec
    def doFacSeries(x: Int, s: Set[Int]): Set[Int] =
      if (s.contains(x)) s else {
        val newX = digits(x).map(fac).sum
        doFacSeries(newX, s + x)
      }

    doFacSeries(n, Set[Int]())
  }


  println ("Solution: " + solution)
}

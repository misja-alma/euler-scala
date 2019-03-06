package euler

import ContinuedFraction._

object Euler64 extends App {

  val solution = (2 to 10000).map(getSquareRootContinuedFraction).map { cf =>
    if (Math.abs(cf.evaluate(25) - Math.sqrt(cf.n)) > 0.0001) {
      println (s"$cf -------------------------------------------------------------> Error ! <------------------------------------")
    }
    cf
  }.count(_.period % 2L == 1)

  println ("Solution: " + solution)
}

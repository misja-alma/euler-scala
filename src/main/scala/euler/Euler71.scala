package euler

object Euler71 extends App {
  // Start with some fraction right below 3/7. Say 2/5. Then:
  // 1. Increase numerator until fraction is above. The last fraction before that is a candidate, it can be added if it can't be simplified
  // 2. Then increase the denominator until fraction is below 3/7 again. Then go to 1.
  // Repeat until denominator > 1000000

  val subject = Fraction(3, 7)

  case class State(lastFraction: Fraction[Int], candidates: Seq[Fraction[Int]])

  val iterator = Iterator.iterate(State(Fraction(2, 5), Seq())) { case State(last, result) =>
    if (last < subject) {
      val newF = Fraction(last.numerator + 1, last.denominator)
      if (newF > subject) {
        if (last.simplify.numerator == last.numerator) {
          State(newF, last +: result)
        } else {
          State(newF, result)
        }
      } else {
        State(newF, result)
      }
    } else {
      val newF = Fraction(last.numerator, last.denominator + 1)
      State(newF, result)
    }
  }

  val State(_, solutions) = iterator.dropWhile(_.lastFraction.denominator <= 1000000).next()
  val solution = solutions.minBy(f => 3.0 / 7 - f.toDouble)

  println ("Solution for problem 71: " + solution)
}

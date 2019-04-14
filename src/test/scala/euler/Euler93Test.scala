package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler93Test extends FlatSpec with Matchers {
  import Euler93._

  "createAllExpressionsWith4PlaceHolders" should "return all expressions with 4 placeholders" in {
    val actualValues = Seq(1, 2, 3, 4).toArray

    val result = createAllExpressions(4)

    // test: take all permutations of the above array, collect all calculated values for all expressions and check that 1 to 28 can be formed.
    val allResults = actualValues.indices.permutations.flatMap { useIndices =>
      useIndices.zipWithIndex.foreach { case (i, number) => {
        actualValues(i) = number + 1
      }}
      result.map(_.calculate(actualValues: _*))
        .filterNot(x => x.denominator == 0 || x.numerator == 0)
        .map(_.simplify)
        .filter(_.denominator == 1)
        .map(_.numerator)
    }.toSet

    longestSequence(allResults) should be (1, 28)
  }
}

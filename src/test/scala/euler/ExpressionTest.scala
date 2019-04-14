package euler

import org.scalatest.{FlatSpec, Matchers}

class ExpressionTest extends FlatSpec with Matchers {
  import Expression._

  "parseExpression" should "parse single placeholder" in {
    val result = parseExpression("$1")

    result.calculate(0, 1, 2).toIntegral should be(1)

    val result2 = parseExpression("$2")

    result2.calculate(0, 1, 2).toIntegral should be(2)
  }

  "parseExpression" should "parse simple expression" in {
    val result = parseExpression("$1+$2")

    result.toString should be("($1+$2)")
    result.calculate( 0, 1, 2, 3).toIntegral should be(3)

    val result2 = parseExpression("$1+$2-$3")

    result2.toString should be("($1+$2-$3)")
    result2.calculate( 0, 1, 2, 3).toIntegral should be(0)

    val result3 = parseExpression("$1+$2*$3")

    result3.toString should be("($1+$2*$3)")
    result3.calculate( 0, 1, 2, 3).toIntegral should be(7)

    val result4 = parseExpression("$1+$2/$3")

    result4.toString should be("($1+$2/$3)")
    result4.calculate( 0, 1, 2, 3) should be(Fraction(5, 3))
  }

  "parseExpression" should "parse expression containing parentheses" in {
    val result = parseExpression("($1+$2)")

    result.toString should be("($1+$2)")
    result.calculate( 0, 1, 2, 3).toIntegral should be(3)

    val result2 = parseExpression("($1+$2)*($0-$3)")

    result2.toString should be("(($1+$2)*($0-$3))")
    result2.calculate( 0, 1, 2, 3).toIntegral should be(-9)

    val result3 = parseExpression("$1-($2+$3)")

    result3.toString should be("($1-($2+$3))")
    result3.calculate( 0, 1, 2, 3).toIntegral should be(-4)
    result3.calculate( 3, 2, 1, 0).toIntegral should be(1)
  }

  "calculate" should "handle division by zero" in {
    val result = parseExpression("$0/($1-$2)")
    result.calculate(1,2,2).denominator should be(0)

    val result2 = parseExpression("$0+($1/$2)")
    result2.calculate(1,2,0).denominator should be(0)
  }
}

package euler

import org.scalatest.{FlatSpec, Matchers}

class FractionTest extends FlatSpec with Matchers {

  "+" should "return the sum of fractions" in {
    Fraction(1, 2) + Fraction(2) should be(Fraction(5, 2))
    Fraction(1, 3) + Fraction(1, 4) should be(Fraction(7, 12))
    Fraction(2, 3) + Fraction(2, 6) should be(Fraction(1))
  }

  "-" should "return the difference of fractions" in {
    Fraction(1, 2) - Fraction(2) should be(Fraction(-3, 2))
  }

  "*" should "return the product of fractions" in {
    Fraction(2, 3) * Fraction(4, 5) should be(Fraction(8, 15))
  }

  "/" should "return the quotient of fractions" in {
    Fraction(2, 3) / Fraction(4, 5) should be(Fraction(10, 12))
  }

  "simplify" should "return the simplified fraction" in {
    Fraction(2, 3).simplify should be(Fraction(2, 3))
    Fraction(12, 10).simplify should be(Fraction(6, 5))
    Fraction(-20, 20).simplify should be(Fraction(-1))
  }

  "compareTo" should "compare the fractions" in {
    Fraction(2, 3) compareTo Fraction(1, 3) should be(1)
    Fraction(2, 3) compareTo Fraction(3, 3) should be(-1)
    Fraction(2, 3) compareTo Fraction(4, 6) should be(0)
  }
}

package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler89Test extends FlatSpec with Matchers {
  import Euler89._

  "parseRoman" should "parse Roman numbers" in {
    parseRoman("X") should be(10)
    parseRoman("IIII") should be(4)
    parseRoman("XI") should be(11)
    parseRoman("IX") should be(9)
    parseRoman("MCDLXXXIV") should be(1484)
  }

  "toRoman" should "generate an optimal Roman number" in {
    toRoman(1) should be("I")
    toRoman(4) should be("IV")
    toRoman(99) should be ("XCIX")
    toRoman(1469) should be ("MCDLXIX")
  }
}

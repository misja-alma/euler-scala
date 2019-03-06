package euler

import org.scalatest.{FlatSpec, Matchers}

class CardTest extends FlatSpec with Matchers {
  import Card._

  "parseCard" should "return a card with correct color and rank" in {
    parseCard("3C") should be(Card(CLUBS, 3))
    parseCard("TD") should be(Card(DIAMONDS, 10))
    parseCard("JH") should be(Card(HEARTS, 11))
    parseCard("QS") should be(Card(SPADES, 12))
    parseCard("KC") should be(Card(CLUBS, 13))
    parseCard("ac") should be(Card(CLUBS, 14))
  }
}



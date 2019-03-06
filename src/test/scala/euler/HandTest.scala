package euler

import org.scalatest.{FlatSpec, Matchers}
import Hand._

class HandTest extends FlatSpec with Matchers {

  "isStraight" should "correctly recognize straights" in {
    parseHand("3c 2c 4c 5c 6d").isStraight should be(true)
    parseHand("2c 3c 4c 5c 8c").isStraight should be(false)
    parseHand("Tc Jc Qc Ac Kc").isStraight should be(true)
    parseHand("2c 3c 4c 5c Ac").isStraight should be(true)
  }

  "isFlush" should "correctly recognize flushes" in {
    parseHand("3c 2c 4c 5c 6c").isFlush should be(true)
    parseHand("3s 2s 4s 5s 6s").isFlush should be(true)
    parseHand("3c 2c 4c 5c 6d").isFlush should be(false)
  }

  "maxStraightRank" should "return the rank of the highest straight card" in {
    parseHand("4c 5c 6c 7c 8d").maxStraightRank should be(8)
    parseHand("Tc 9c 8c 7c 6d").maxStraightRank should be(10)
  }

  "pairCount" should "return the counts of every pair or trip" in {
    parseHand("4c 5c 6c 7c 8d").pairCount should be((0, 0))
    parseHand("4d 4c 4s 7c 4h").pairCount should be((4, 0))
    parseHand("5d 5c 5h 7c 8d").pairCount should be((3, 0))
    parseHand("4c 4h 6c 7c 8d").pairCount should be((2, 0))
    parseHand("4c 4h 6c 7c 6d").pairCount should be((2, 2))
    parseHand("4c 4h 6c 6s 6d").pairCount should be((3, 2))
  }

  def parseCards(s: String): Array[Card] = s.split(" ").map(Card.parseCard)

  "notInPair" should "return the cards that are not in any pair" in {
    parseHand("4c 5c 6c 7c 8d").notInPair.map(_.rank).sorted should be(parseHand("4c 5c 6c 7c 8d").cards.map(_.rank).sorted)
    parseHand("4c 4d 6c 7c 8d").notInPair.map(_.rank).sorted should be(parseCards("6c 7c 8d").map(_.rank).sorted)
    parseHand("4c 4d 6c 6c 8d").notInPair.map(_.rank).sorted should be(parseCards("8d").map(_.rank).sorted)
    parseHand("4c 4d 4h 4s 8d").notInPair.map(_.rank).sorted should be(parseCards("8d").map(_.rank).sorted)
  }
}

package euler

import scala.io.Source

object Euler54 extends App {
  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p054_poker.txt")).getLines().toList

  def parseTwoHands(s: String): (Hand, Hand) = {
    val (h1, h2) = s.splitAt(15)
    (Hand.parseHand(h1), Hand.parseHand(h2))
  }

  val p1Wins = lines.map(parseTwoHands).count { case (h1, h2) => HandRanking.rankHand(h1).compareTo(HandRanking.rankHand(h2)) > 0}

  println ("Solution: " + p1Wins)
}

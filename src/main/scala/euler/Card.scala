package euler

import scala.annotation.tailrec
import scala.util.Random

object Card {
  val CLUBS = 0
  val DIAMONDS = 1
  val HEARTS = 2
  val SPADES = 3

  val RANK_MODULUS = 15

  def parseColor(c: Char): Int = c.toUpper match {
    case 'C' => CLUBS
    case 'D' => DIAMONDS
    case 'H' => HEARTS
    case 'S' => SPADES
    case _ => sys.error("Not a color: " + c)
  }

  def parseRank(c: Char): Int = c.toUpper match {
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
    case x if x >= '2' && x <= '9' => x - '0'
    case _ => sys.error("Not a rank: " + c)
  }

  def parseCard(s: String): Card = Card(parseColor(s(1)), parseRank(s.head))

  /**
    * Returns an array with nrCards shuffled indices
    */
  def shuffle(nrCards: Int): Array[Int] = {
    def swap(i1: Int, i2: Int, ar: Array[Int]): Unit = {
      val h = ar(i1)
      ar(i1) = ar(i2)
      ar(i2) = h
    }

    @tailrec
    def doShuffle(i: Int, cards: Array[Int]): Unit = {
      if (i < cards.length) {
        val rnd = Random.nextInt(cards.length - i)
        swap(cards.length - i - 1, rnd, cards)
        doShuffle(i + 1, cards)
      }
    }

    val result = (0 until nrCards).toArray
    doShuffle(0, result)
    result
  }
}

case class Card(color: Int, rank: Int)
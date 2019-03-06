package euler

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
}

case class Card(color: Int, rank: Int)
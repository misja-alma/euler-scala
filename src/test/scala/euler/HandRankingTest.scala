package euler

import org.scalatest.{FlatSpec, Matchers}
import Hand._

class HandRankingTest extends FlatSpec with Matchers {

  "rankHand" should "return the correct ranking" in {
    rank("Ts Tc Td Th Ks").getClass should be(classOf[Carre])
    rank("Ts Tc Td 3h Ks").getClass should be(classOf[Trips])
    rank("Ts Tc Td Kh Ks").getClass should be(classOf[FullHouse])
    rank("Ts Tc 2d Kh Ks").getClass should be(classOf[TwoPair])
    rank("Ts Tc 2d 9h Ks").getClass should be(classOf[OnePair])
    rank("Ts Qd 2d 9h Ks").getClass should be(classOf[HighCard])

    rank("Ts 9d 8d 7h 6s").getClass should be(classOf[Straight])
    rank("Ts Qs 2s 9s Ks").getClass should be(classOf[Flush])
    rank("As 2s 3s 4s 5s").getClass should be(classOf[StraightFlush])

    rank("Ts Qd Jd 9h Ah").getClass should be(classOf[HighCard])
    rank("As 3d 4d 5d 6d").getClass should be(classOf[HighCard])
  }

  "compareTo" should "correctly recognize the best hand" in {
    rank("Ts Tc 2s 2c Ks") compareTo rank("Ks Kh As Js 3s") should be(1)
    rank("Ts Tc 2s 2c Ks") compareTo rank("Ks Kh As Ah 3s") should be(-1)
    rank("Ts Tc 2s 2c Ks") compareTo rank("Th Td 2h 2d As") should be(-1)
    rank("Ts Tc 2s 2c Ks") compareTo rank("3h 3d 3s 2d As") should be(-1)

    rank("As 3d 4d 5d 6d") compareTo rank("Ts Qd Jd 9h Ah") should be(-1)
    rank("As Kh Qd Js Th") compareTo rank("9h Th Jh Qs Ks") should be(1)
    rank("2s 3s 4s 5s 6h") compareTo rank("As 3h 4h 5d 2d") should be(1)

    rank("Ks 3s 4s 5s 2s") compareTo rank("Ts Qs Js 9s 7s") should be(1)
  }

  def rank(s: String): HandRanking = HandRanking.rankHand(parseHand(s))

}

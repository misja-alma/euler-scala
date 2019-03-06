package euler

import Card.RANK_MODULUS

object HandRanking {
  def rankHand(hand: Hand): HandRanking = hand match {
    case StraightFlush(sf) => sf
    case Carre(carre)      => carre
    case FullHouse(fh)     => fh
    case Flush(fl)         => fl
    case Straight(st)      => st
    case Trips(tr)         => tr
    case TwoPair(tp)       => tp
    case OnePair(op)       => op
    case _                 => HighCard(hand)
  }
}

trait HandRanking extends Comparable[HandRanking] {
  def typeRank: Int
  def rank: Int

  override def compareTo(o: HandRanking): Int = {
    val result = typeRank.compareTo(o.typeRank)
    if (result == 0) {
      rank.compareTo(o.rank)
    } else {
      result
    }
  }
}

object Hand {
  // Expects a string containing 5 cards separated with spaces
  def parseHand(s: String): Hand = Hand(s.split(" ").map(Card.parseCard))

  def rankCards(cards: Array[Card]): Int = cards.map(_.rank).sorted.reverse.foldLeft(0) {
    case (sum, c) =>  RANK_MODULUS * sum + c
  }
}

case class Hand(cards: Array[Card]) {
  private lazy val sortedRanks = cards.map(_.rank).sorted
  private lazy val countPerRank = cards.groupBy(_.rank).mapValues(_.length)

  // TODO should have boolean that indicates if low straight is allowed. For now we allow it.
  lazy val maxStraightRank: Int = {
    val last = sortedRanks.last
    if (last != 14) last else if (sortedRanks.head == 2) 5 else 14
  }
  lazy val isStraight: Boolean = {
    sortedRanks.toSet.size == 5 &&
      (maxStraightRank - sortedRanks.head == 4 || (maxStraightRank == 5 && (sortedRanks(3) - sortedRanks.head == 3)))
  }

  lazy val isFlush: Boolean = cards.map(_.color).toSet.size == 1

  lazy val pairCount: (Int, Int) = (countPerRank.getOrElse(highPairRank, 0), countPerRank.getOrElse(lowPairRank, 0))
  // Either the higher pair, or in case of full house, the trip. For both: if no pair, then they have value 0
  lazy val highPairRank: Int = {
    val pairs = countPerRank.filter(_._2 >= 2)
    if (pairs.isEmpty) 0 else if (pairs.size == 1) pairs.head._1 else {
      // 2 pair or full house
      val (rank1, size1) = pairs.head
      val (rank2, size2) = pairs.last
      if (size1 == 3) rank1 else if (size2 == 3) rank2 else if (rank1 > rank2) rank1 else rank2
    }
  }
  lazy val lowPairRank: Int = countPerRank
    .filterKeys(_ != highPairRank) // not the high pair
    .filter(_._2 == 2)             // find group with size 2
    .keys.headOption.getOrElse(0)  // take the key (the rank) or otherwise 0
  lazy val notInPair: Array[Card] = cards.filterNot(c => c.rank == highPairRank || c.rank == lowPairRank)
}

object StraightFlush {
  def unapply(h: Hand): Option[StraightFlush] = if (h.isStraight && h.isFlush) Some(StraightFlush(h)) else None
}

case class StraightFlush(hand: Hand) extends HandRanking {
  val typeRank = 8
  lazy val rank = hand.maxStraightRank
}

object Carre {
  def unapply(h: Hand): Option[Carre] = if (h.pairCount._1 == 4) Some(Carre(h)) else None
}

case class Carre(hand: Hand) extends HandRanking {
  val typeRank = 7
  lazy val rank = hand.highPairRank * RANK_MODULUS + Hand.rankCards(hand.notInPair)
}

object FullHouse {
  def unapply(h: Hand): Option[FullHouse] = if (h.pairCount._1 == 3&& h.pairCount._2 == 2) Some(FullHouse(h)) else None
}

case class FullHouse(hand: Hand) extends HandRanking {
  val typeRank = 6
  lazy val rank = hand.highPairRank * RANK_MODULUS + hand.lowPairRank
}

object Flush {
  def unapply(h: Hand): Option[Flush] = if (h.isFlush) Some(Flush(h)) else None
}

case class Flush(hand: Hand) extends HandRanking {
  val typeRank = 5
  lazy val rank = Hand.rankCards(hand.cards)
}

object Straight {
  def unapply(h: Hand): Option[Straight] = if (h.isStraight) Some(Straight(h)) else None
}

case class Straight(hand: Hand) extends HandRanking {
  val typeRank = 4
  lazy val rank = hand.maxStraightRank
}

object Trips {
  def unapply(h: Hand): Option[Trips] = if (h.pairCount._1 == 3) Some(Trips(h)) else None
}

case class Trips(hand: Hand) extends HandRanking {
  val typeRank = 3
  lazy val rank = hand.highPairRank * RANK_MODULUS * RANK_MODULUS + Hand.rankCards(hand.notInPair)
}

object TwoPair {
  def unapply(h: Hand): Option[TwoPair] = if (h.pairCount._1 == 2 && h.pairCount._2 == 2) Some(TwoPair(h)) else None
}

case class TwoPair(hand: Hand) extends HandRanking {
  val typeRank = 2
  lazy val rank = hand.highPairRank * RANK_MODULUS * RANK_MODULUS + hand.lowPairRank * RANK_MODULUS + Hand.rankCards(hand.notInPair)
}

object OnePair {
  def unapply(h: Hand): Option[OnePair] = if (h.pairCount._1 == 2) Some(OnePair(h)) else None
}

case class OnePair(hand: Hand) extends HandRanking {
  val typeRank = 1
  lazy val rank = hand.highPairRank * RANK_MODULUS * RANK_MODULUS * RANK_MODULUS + Hand.rankCards(hand.notInPair)
}

case class HighCard(hand: Hand) extends HandRanking {
  val typeRank = 0
  lazy val rank = Hand.rankCards(hand.cards)
}


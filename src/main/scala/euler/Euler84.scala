package euler

import Card._
import scala.util.Random

object Euler84 extends App {
  // a game simulation can be an iterator. At the -end- of every move, increase the count of the new square.
  // Note: we could run 1 long simulation, or a bunch of smaller ones.
  // so this means that the g2j and the ch and cc squares have probability 0. The jail not, because it is assumed that you leave next turn.
  // special squares are:
  // 2, 17, 33 (community chest)
  // 7, 22, 36 (chance)
  // 30 (go to jail)
  // 5, 15, 25, 35: Railway company
  // 12, 28: Utility company

  case class State(square: Int, doubles: Int, turns: Int, ch: Vector[Int], cc: Vector[Int])

  type StateTransfomer = State => State

  def setSquare(s: Int): StateTransfomer = _.copy(square = s)

  def nextR: StateTransfomer = s => s.copy(square = (s.square + 10 - (s.square + 5) % 10) % 40)

  def nextU: StateTransfomer = s => s.copy(square = if (s.square >= 28 || s.square < 12) 12 else 28)

  def back3: StateTransfomer = s => s.copy(square = (s.square - 3 + 40) % 40)

  def other: StateTransfomer = identity

  // Note that for both decks, the non-movement cards are included.
  val ccDestinations = (Seq(setSquare(0), setSquare(10)) ++ Seq.fill(14)(other)).toArray
  val chDestinations = (Seq(setSquare(0), setSquare(10), setSquare(11), setSquare(24), setSquare(39), setSquare(5), nextR, nextR, nextU, back3) ++ Seq.fill(6)(other)).toArray

  val squareCounts = new Array[Int](40)

  val initState = State(0, 0, 0, shuffle(chDestinations.length).toVector, shuffle(ccDestinations.length).toVector)

  val _ = Iterator.iterate(initState)(move).dropWhile(_.turns < 1000000).next()

  def rollDice: (Int, Int) = (Random.nextInt(4) + 1, Random.nextInt(4) + 1)

  def communityChest(state: State, nextSquare: Int): State = {
    val card = state.cc.head
    val newDeck = state.cc.tail :+ card
    ccDestinations(card)(state.copy(square = nextSquare, cc = newDeck))
  }

  def chance(state: State, nextSquare: Int): State = {
    val card = state.ch.head
    val newDeck = state.ch.tail :+ card
    chDestinations(card)(state.copy(square = nextSquare, ch = newDeck))
  }

  def move(s: State): State = {
    val (die1, die2) = rollDice
    val isDouble = die1 == die2
    val (nextSquare, nextDoubles) =
      if (isDouble) {
        if (s.doubles == 2) {
          (10, 0)
        } else {
          ((s.square + die1 + die2) % 40, s.doubles + 1)
        }
      } else ((s.square + die1 + die2) % 40, s.doubles + 1)

    val newState: State = nextSquare match {
      case 2 | 17 | 33 => communityChest(s, nextSquare)
      case 7 | 22 | 36 => chance(s, nextSquare)
      case 30 => s.copy(square = 10) // NOTE: I assume that doubles should not be reset here ?
      case _ => s.copy(square = nextSquare)
    }

    squareCounts(newState.square) += 1

    newState.copy(doubles = nextDoubles, turns = newState.turns + 1)
  }

  val top3 = squareCounts.zipWithIndex.sortBy(_._1).map(_._2).reverse.take(3)

  def to2Digits(x: Int): String = if (x >= 10) x.toString else "0" + x

  println ("Solution: " + top3.map(to2Digits).mkString)

}

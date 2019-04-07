package euler

import scala.io.Source
import Utils._

object Euler89 extends App {
  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p089_roman.txt")).getLines().toList

  def addToResults(lastResults: Seq[Int], nextInt: Int): Seq[Int] =
    if (lastResults.nonEmpty && lastResults.head < nextInt) (nextInt - lastResults.head) +: lastResults.tail else nextInt +: lastResults

  def parseNextChar(lastResults: Seq[Int], nextChar: Char): Seq[Int] = nextChar match {
    case 'I' => 1 +: lastResults
    case 'V' => addToResults(lastResults, 5)
    case 'X' => addToResults(lastResults, 10)
    case 'L' => addToResults(lastResults, 50)
    case 'C' => addToResults(lastResults, 100)
    case 'D' => addToResults(lastResults, 500)
    case 'M' => addToResults(lastResults, 1000)
    case _ => sys.error("Unknown char: " + nextChar)
  }

  lazy val oneAtPos = Map[Int, Char] (
    0 -> 'I',
    1 -> 'X',
    2 -> 'C',
    3 -> 'M'
  )

  lazy val fiveAtPos = Map[Int, Char] (
    0 -> 'V',
    1 -> 'L',
    2 -> 'D'
  )

  def parseRoman(str: String): Int = str.foldLeft(Seq[Int]())(parseNextChar).sum

  def intToRoman(pos: Int, x: Int): String = {
    lazy val d = oneAtPos(pos)
    lazy val f = fiveAtPos(pos)
    lazy val dn = oneAtPos(pos + 1)
    x match {
      case 0 => ""
      case 1 => s"$d"
      case 2 => s"$d$d"
      case 3 => s"$d$d$d"
      case 4 => if (pos >= 3) s"$d$d$d$d" else s"$d$f"
      case 5 => s"$f"
      case 6 => s"$f$d"
      case 7 => s"$f$d$d"
      case 8 => s"$f$d$d$d"
      case 9 => s"$d$dn"
    }
  }

  def toRoman(x: Int): String =
    digits(x)
      .reverse
      .zipWithIndex
      .foldLeft(""){ case (result, (d, position)) => intToRoman(position, d) ++ result }

  def optimizeRoman(r: String): String = {
    val parsed = parseRoman(r)
    toRoman(parsed)
  }

  val saved = lines.map(line => (line.length, optimizeRoman(line).length)).map(sizes => sizes._1 - sizes._2).sum

  println ("Solution: " + saved)
}

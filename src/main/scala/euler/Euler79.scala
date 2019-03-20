package euler

import scala.io.Source

object Euler79 extends App {
  // for each digit, keep both its average position and its max position

  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p079_keylog.txt")).getLines().toList

  case class DigitData(totalPos: Int, maxPos: Int, freq: Int, char: Character)

  val data = new Array[DigitData](10)

  lines.foreach { line => {
    line.zipWithIndex.foreach { case (c, i) =>
      val index = c - '0'
      val existing = data(index)
      if (existing == null) {
        data(index) = DigitData(i, i, 1, c)
      } else {
        data(index) = DigitData(existing.totalPos + i, Math.max(existing.maxPos, i), existing.freq + 1, c)
      }
    }
  }}

  val sorted = data.filterNot(_ == null).sortBy(d => d.totalPos.toDouble / d.freq)

  // It turns out this naive solution is already enough, max. pos is not even needed.
  println ("Solution: " + sorted.map(_.char).mkString)
}

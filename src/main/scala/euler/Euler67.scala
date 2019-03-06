package euler

import scala.io.Source

object Euler67 extends App {
  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p067_triangle.txt")).getLines().toList

  val grid: Array[Array[Int]] = lines.map(_.split(" ").map(_.toInt)).toArray

  def upperNeighbours(r: Int, c: Int): Seq[Int] = {
    if (r == 0) Seq() else
      if (c == 0) Seq(grid(r-1)(c)) else
        if (c == grid(r).length - 1) Seq(grid(r-1)(c-1)) else
          Seq(grid(r-1)(c-1), grid(r-1)(c))
  }
  
  grid.indices.tail.foreach { row => {
    grid(row).indices.foreach { col => {
      grid(row)(col) += upperNeighbours(row, col).max
    }}
  }}

  val longestPath = grid.last.max

  println ("Solution: " + longestPath)
}

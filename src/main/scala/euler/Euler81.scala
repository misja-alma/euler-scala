package euler

import scala.io.Source

object Euler81 extends App {

  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p081_matrix.txt")).getLines().toList

  val shortestPaths = new Array[Array[Int]](80).map(_ => new Array[Int](80))

  val grid = lines.map(_.split(",").map(_.toInt).toList)

  def selectShortest(pos1: (Int, Int), pos2: (Int, Int)): Option[Int] = {
    val (row1, col1) = pos1
    val (row2, col2) = pos2
    val s1 = if (row1 >= 0 && col1 >= 0) Some(shortestPaths(row1)(col1)) else None
    val s2 = if (row2 >= 0 && col2 >= 0) Some(shortestPaths(row2)(col2)) else None

    val shortest = for {
      p1 <- s1
      p2 <- s2
    } yield Math.min(p1, p2)

    shortest.orElse(s1 orElse s2)
  }

  for {
    row <- 0 until 80
    col <- 0 until 80
  } yield {
    val me = grid(row)(col)
    val shortestSoFar = selectShortest((row, col - 1), (row - 1, col)).getOrElse(0)
    shortestPaths(row)(col) = me + shortestSoFar
  }

  println ("Solution: " + shortestPaths(79)(79))
}

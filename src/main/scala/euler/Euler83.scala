package euler

import scala.collection.mutable
import scala.io.Source

object Euler83 extends App {
  // Just an A* search, the same as in Euler82. The only modifications are the starting points (this time the top left corner), the end condition (top right  corner),
  // the heuristic (with same lenghts, manhattan distance to endpoint breaks the tie) and the nr of new neighbours.

  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p083_matrix.txt")).getLines().toList

  // init a 80x80 shortestPaths array with int.maxValue
  val shortestPaths = Array.fill(80)(Array.fill(80)(Int.MaxValue))

  val grid = lines.map(_.split(",").map(_.toInt).toList)

  case class Path(length: Int, row: Int, col: Int, visited: Set[(Int, Int)])

  def distToEndPoint(path: Path): Int = 79 - path.col + 79 - path.row

  implicit val pathOrdering = new Ordering[Path] {
    override def compare(x: Path, y: Path): Int = {
      val res = y.length.compareTo(x.length) // smaller length is better
      if (res == 0) distToEndPoint(y).compareTo(distToEndPoint(x)) else res // lower manhattan distance to endpoint breaks the tie
    }
  }

  val pq = new mutable.PriorityQueue[Path]()

  // add the left top corner to pq as it is
  val topLeftPath = Path(grid(0)(0), 0, 0, Set((0, 0)))
  pq.enqueue(topLeftPath)
  shortestPaths(topLeftPath.row)(topLeftPath.col) = topLeftPath.length

  var solution: Option[Path] = None

  while (pq.nonEmpty && solution.isEmpty) {
    val next = pq.dequeue()
    if (next.col == 79 && next.row == 79) {
      solution = Some(next)
    } else {
      val newNeighbours = neighbours(next)
      newNeighbours.foreach(p => shortestPaths(p.row)(p.col) = p.length)
      pq.enqueue(newNeighbours: _*)
    }
  }

  def neighbours(path: Path): Seq[Path] =
    Seq(
      safeNextPath(path.length, path.visited, path.row, path.col + 1),
      safeNextPath(path.length, path.visited, path.row, path.col - 1),
      safeNextPath(path.length, path.visited, path.row - 1, path.col),
      safeNextPath(path.length, path.visited, path.row + 1, path.col),
    )
      .flatten
      .filterNot(p => shortestPaths(p.row)(p.col) <= p.length)

  def safeNextPath(length: Int, visited: Set[(Int, Int)], row: Int, col: Int): Option[Path] =
    if (row < 0 || row > 79 || col < 0 || col > 79 || visited.contains((row, col))) None else Some(Path(length + grid(row)(col), row, col, visited + (row -> col)))

  println("Solution: " + solution.get.length)
}

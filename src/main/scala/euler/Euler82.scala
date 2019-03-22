package euler

import scala.collection.mutable
import scala.io.Source

object Euler82 extends App {
  // Just an A* search. Use a PQ that simply selects the shortest path so far and with same lengths the furthest column. Stop when last column reached; take care not to have cycles.
  // On top of that, the algo could keep the paths array to keep track of the shortest found path so far; this way we can prune slow paths.

  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p082_matrix.txt")).getLines().toList

  // init a 80x80 shortestPaths array with int.maxValue
  val shortestPaths = Array.fill(80)(Array.fill(80)(Int.MaxValue))

  val grid = lines.map(_.split(",").map(_.toInt).toList)

  case class Path(length: Int, row: Int, col: Int, visited: Set[(Int, Int)])

  implicit val pathOrdering = new Ordering[Path] {
    override def compare(x: Path, y: Path): Int = {
      val res = y.length.compareTo(x.length) // smaller length is better
      if (res == 0) x.col.compareTo(y.col) else res // higher col breaks the tie
    }
  }

  val pq = new mutable.PriorityQueue[Path]()

  // add the first col to pq as it is
  grid.map(_.head).zipWithIndex.map { case (v, row) => Path(v, row, 0, Set((row, 0))) }.foreach(path => {
    pq.enqueue(path)
    shortestPaths(path.row)(path.col) = path.length
  })

  var solution: Option[Path] = None

  while (pq.nonEmpty && solution.isEmpty) {
    val next = pq.dequeue()
    if (next.col == 79) {
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
      safeNextPath(path.length, path.visited, path.row - 1, path.col),
      safeNextPath(path.length, path.visited, path.row + 1, path.col),
    )
    .flatten
    .filterNot(p => shortestPaths(p.row)(p.col) <= p.length)

  def safeNextPath(length: Int, visited: Set[(Int, Int)], row: Int, col: Int): Option[Path] =
    if (row < 0 || row > 79 || col < 0 || col > 79 || visited.contains((row, col))) None else Some(Path(length + grid(row)(col), row, col, visited + (row -> col)))

  println("Solution: " + solution.get.length)
}

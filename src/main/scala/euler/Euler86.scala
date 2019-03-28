package euler

object Euler86 extends App {
  // Use the fact then when the cube is unfolded, the shortest route is a straight line between the two corners
  // So L = Sqrt((a + b)^2 + c^2) for each combination of a, b and c in the (a+b) part.
  // To find the solutions for a given M, we know that M*M = (a+b)^2 + c^2 for the combination a,b,c that gives the shortest route.
  // The route is the shortest when the longest of a,b,c is c.
  // So for each c, count all a,b pairs that give the remaining square and where both are <= c. Note that we want distinct cuboids so probably we need to keep them in a Set.

  case class State(m: Int, solutions: Int)

  def isSquare(x: Int): Boolean = {
    val s = Math.sqrt(x).toInt
    s * s == x
  }

  def shortestPaths(c: Int): Int =
    (1 to c).map { a =>
      (a to c).count(b => isSquare((a + b) * (a + b) + c * c))
    }.sum

  def addNextSolutions(state: State): State = {
    val nextM = state.m + 1
    State(nextM, state.solutions + shortestPaths(nextM))
  }

  val State(m, solutions) = Iterator.iterate(State(1, 0))(addNextSolutions).dropWhile(_.solutions <= 1000000).next

  println ("Solution: " + m)
}

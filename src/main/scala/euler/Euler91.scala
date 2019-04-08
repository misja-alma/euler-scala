package euler

object Euler91 extends App {

  type Triangle = Seq[Point]
  type Line = (Point, Point)
  type Point = (Int, Int)
  
  def trianglesTouchingBorder(border: Int): Seq[Triangle] = {
    // Enumerate all triangles which have a point in the origin and at least one on the border.
    // For each point on the border, add all third points both to left and right, so basically all of them except origin and point itself and anything on the line in between.
    // Only don't count border points to the left to prevent counting them double.

    val touchingTop = (0 to border).flatMap { xTop =>
      val topPoint = (xTop, border)
      for {
        x <- 0 to border
        y <- 0 to border
        if !(y == border && x < xTop) && !onSameLineToOrigin((x, y), topPoint)
      } yield Seq((0, 0), topPoint, (x, y))
    }

    val touchingRight = (0 to border).flatMap { yRight =>
      val rightPoint = (border, yRight)
      for {
        x <- 0 to border
        y <- 0 to border
        if y != border && !(x == border && y < yRight) && !onSameLineToOrigin((x, y), rightPoint)
      } yield Seq((0, 0), rightPoint, (x, y))
    }

    touchingTop ++ touchingRight
  }

  def onSameLineToOrigin(p1: Point, p2: Point): Boolean = p1._2 * p2._1 - p1._1 * p2._2 == 0

  def rightAngle(line1: Line, line2: Line): Boolean = {
    val vec1 = (line1._2._1 - line1._1._1, line1._2._2 - line1._1._2)
    val vec2 = (line2._2._1 - line2._1._1, line2._2._2 - line2._1._2)
    vec1._1 * vec2._1 + vec1._2 * vec2._2 == 0
  }

  def isRightTriangle(t: Triangle): Boolean = {
    val line1 = (t(0), t(1))
    val line2 = (t(1), t(2))
    val line3 = (t(0), t(2))
    rightAngle(line1, line2) || rightAngle(line1, line3) || rightAngle(line2, line3)
  }

  val triangles = for {
    b <- 1 to 50
    t <- trianglesTouchingBorder(b)
  } yield t

  println ("Solution: " + triangles.count(isRightTriangle))
}

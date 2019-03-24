package euler

object Euler85 extends App {
  // Combine rectangles using the formula Count(A, B) = Count(A) + Count(B) + width(A).width(B).Count(B)
  // If we add a block of width 1 every time, then Count(A) has a recurrence:
  // Count(k) = Count(k - 1) + Count(1) + (k-1).Count(1) = Count(k - 1) + k.Count(1) = k.(k+1)/2 * Count(1)
  // Note that Count(1) depends on the height that was chosen for the rectangles
  // Algo: for every height starting at one, iterate through the series until the first value > 2000000 is found. Keep the max of this and the previous value, together with total width and height
  // Then take the next height, until a row is found where 2 million is exceeded while still width <= height.
  // Take the min(Abs(2000000 - count)) of all rows, the area is the solution.

  case class RectangleResult(rCount: Int, width: Int, height: Int) {
    def area: Int = width * height
  }

  def calcCount(width: Int, countOfWidthOne: Int): Int = width * (width + 1) / 2 * countOfWidthOne

  def findClosestRectangleCountWithHeight(height: Int, threshold: Int): RectangleResult = {
    val widthOne = calcCount(height, 1)
    val (w, firstBigger) = Iterator.iterate((1, widthOne)) { case (width, _) =>  (width + 1, calcCount(width + 1, widthOne))}.dropWhile(_._2 < threshold).next()
    val lastSmaller = calcCount(w - 1 , widthOne)
    if (Math.abs(threshold - firstBigger) < Math.abs(threshold - lastSmaller)) {
      RectangleResult(firstBigger, w, height)
    } else {
      RectangleResult(lastSmaller, w - 1, height)
    }
  }

  val threshold = 2000000

  val bestRectangle = Stream.from(1)
    .map(findClosestRectangleCountWithHeight(_, threshold))
    .takeWhile(result => result.width >= result.height)
    .minBy(result => Math.abs(threshold - result.rCount))

  println ("Solution: " + bestRectangle.area)
}

package euler

// 0.5 = b/t * (b-1)/(t-1) where t is the total and b the nr of blue discs
// Writing this out as a square equation and solving for b gives b = (1 + sqrt(1 + 2*t*(t-1)))/2
// Just iterating over 1 > 1 trillion and finding a whole solution for b is too slow.
// Writing out the equation where b is the nr of blue discs and y is the total nr of discs gives:
// 2 ⁢x² - y² - 2 ⁢x + y = 0
// Solving this as a Pell equation on the alpertron website gives:
// x = 1
// y = 1
//
// and also:
// x = 0
// y = 0
//
// Recursive solutions:
//
// xn+1 = 3 ⁢xn + 2 ⁢yn - 2
// yn+1 = 4 ⁢xn + 3 ⁢yn - 3
//
// and also:
//
// xn+1 = 3 ⁢xn - 2 ⁢yn
// yn+1 = - 4 ⁢xn + 3 ⁢yn + 1
object Euler100 extends App {

  def firstRecurrence(x: Long, y: Long):(Long, Long) = (3 * x + 2 * y - 2, 4 * x + 3 * y - 3)

  def secondRecurrence(x: Long, y: Long):(Long, Long) = (3 * x - 2 * y, -4 * x + 3 * y + 1)

  val (b1, t1) = Iterator.iterate((1L, 1L)){ case (b, t) =>  firstRecurrence(b, t)}.dropWhile(_._2 <= 1000000000000L).next()
  // The second recurrence gives negative results for b so can be ignored.
  val (b2, t2) = Iterator.iterate((0L, 0L)){ case (b, t) =>  secondRecurrence(b, t)}.dropWhile(_._2 <= 1000000000000L).next()

  println (s"Solution: $b1")
}

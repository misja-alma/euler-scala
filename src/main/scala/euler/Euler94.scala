package euler

object Euler94 extends App {
  // Algo: The triangle needs to have two sides of length a and one side of either a + 1 or a - 1
  // just iterate all a's and check both cases if they lead to a 4 square, sum the perimeters.
  // writing out the formula for a square as a function of a, we get
  // 16*S^2 = (3a+1)(a-1)(a+1)(a+1) for the a + 1 case and
  // 16*S^2 = (3a-1)(a+1)(a-1)(a-1) for the a - 1 case
  // Since the second part of the right side is each time already a square, it suffices to check that
  // either (3a+1)(a-1) or (3a-1)(a+1) is also a square.
  // Note that in both cases, a has to be odd to yield a square that is divisible by 16.

  def isSquare(x: Long): Boolean = {
    val root = Math.sqrt(x + 0.5).toLong
    root * root == x
  }
  
  val aMinusOnePerimeterSum = (3L to 333333333 by 2)   // 1 1 0 is not a triangle
    .filter { a =>
      isSquare((3 * a - 1) * (a + 1))
    }
    .foldLeft(0L){ case (sum, a) => sum + 3 * a - 1}  

  val aPlusOnePerimeterSum = (3L to 333333333 by 2)   // 1 1 2 is not a triangle
    .filter { a =>
      isSquare((3 * a + 1) * (a - 1))
    }
    .foldLeft(0L){ case (sum, a) => sum + 3 * a + 1}

  println ("Solution: " + (aMinusOnePerimeterSum + aPlusOnePerimeterSum))
}

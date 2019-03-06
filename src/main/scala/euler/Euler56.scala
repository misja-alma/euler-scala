package euler

import Utils._

object Euler56 extends App {

  def digitSum(x: BigInt) = digits(x).sum

  val allSums = for {
    x <- 2 until 100
    y <- 2 until 100
  } yield {
    val power = BigInt(x).pow(y)
    digitSum(power)
  }

  val maxSum = allSums.max

  println ("Maximum: " + maxSum)

}

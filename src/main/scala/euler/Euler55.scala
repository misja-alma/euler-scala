package euler

object Euler55 extends App {
  
  def palindrome(i: BigInt): BigInt = BigInt(i.toString.reverse)

  def addToReverse(i: BigInt): BigInt = i + palindrome(i)

  def isPalindrome(i: BigInt): Boolean = palindrome(i) == i

  def isLychrelNumber(i: Long): Boolean = {
    val first50 = Iterator.iterate(BigInt(i))(addToReverse).take(51).toList
    !first50.tail.exists(isPalindrome)
  }

  val result = (1L until 10000).count(isLychrelNumber)

  println ("Nr of Lychrel numbers: " + result)
}

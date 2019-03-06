package euler

object Euler63 extends App {
  val solution = (1 to 50).map(countNrsWithPower).sum

  println ("Solution: " + solution)

  def power(p: Int)(subject: Int): BigInt = {
    var total = BigInt(1)
    (1 to p).foreach(_ => total = total * subject)
    total
  }
  
  def countNrsWithPower(i: Int): Int = {
    (1 to 20).map(power(i)).count(x => Utils.digits(x).size == i)
  }
}

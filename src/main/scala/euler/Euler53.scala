package euler

object Euler53 extends App {

  def fac(from: Int, until: Int): BigInt = if (from == until) 1 else fac(from - 1, until) * from

  def nOverM(n: Int, m: Int): BigInt = fac(n, m) / fac(n - m, 0)

  def countBigCombinatorics(n: Int): Int = (1 to n).map { m => nOverM(n, m) }.count ( _ > 1000000 )

  val solution = (1 to 100).map (countBigCombinatorics).sum

  println(s"Solution: $solution")
}

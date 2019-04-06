package euler

import org.scalatest.{FlatSpec, Matchers}
import Totient._

class TotientTest extends FlatSpec with Matchers {

  implicit lazy val factorCache: Utils.FactorCache[Long] = collection.mutable.Map[Long, Seq[Long]]()
  implicit lazy val primeCache: Utils.PrimeCache[Long] = new Utils.PrimeCache[Long]

  "phi" should "return the totient" in {
    phi(2) should be(1)
    phi(3) should be(2)
    phi(4) should be(2)
    phi(9) should be(6)
    phi(132) should be(40)
  }
}

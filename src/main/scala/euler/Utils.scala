package euler

import scala.annotation.tailrec
import scala.collection.immutable.Set

object Utils {
  def digits(x: BigInt): Seq[Int] = {
    var digits = Seq[Int]()
    var remain = x
    do {
      val digit = remain % 10
      remain /= 10
      digits = digit.toInt +: digits
    } while (remain > 0)
    digits
  }

  def digits(x: Long): Seq[Int] = {
    var digits = Seq[Int]()
    var remain = x
    do {
      val digit = remain % 10
      remain /= 10
      digits = digit.toInt +: digits
    } while (remain > 0)
    digits
  }

  def cross[X, Y](xs: Traversable[X], ys: Traversable[Y]): Traversable[(X, Y)] = for {x <- xs; y <- ys } yield (x, y)

  def concat(x: Int, y: Int): Long = toLong(digits(x) ++ digits(y))

  def toLong(digits: Seq[Int]): Long = {
    @tailrec
    def toLong(left: Long, digits: Seq[Int]): Long = digits match {
      case Seq(x) => left * 10L + x
      case ds => toLong(left * 10L + ds.head, ds.tail)
    }

    toLong(0, digits)
  }

  def square(n: Long): Long = n * n

  def divides(numerator: Long)(divider: Long): Boolean = numerator % divider == 0

  def pairs[T](s: Set[T]): Set[(T, T)] = s match {
    case s if s.isEmpty => Set()
    case _ => s.tail.map(x => (s.head, x)) ++ pairs(s.tail)
  }

  class PrimeCache[T](val two: T, val three: T)(implicit ordering: Ordering[T]) {
    private val cache = collection.mutable.SortedSet[T](two, three)

    def contains(x: T): Boolean = cache.contains(x)

    def last: T = cache.lastKey
    
    def iterator: Iterator[T] = cache.iterator
    
    def add(x: T) = cache += x
  }

  def primes(implicit cache: PrimeCache[Long]): Iterator[Long] = {
    val largestKnown = cache.last
    // TODO could be optimized even more, because this assumes that cache will only be updated within this method
    // In reality it could be updated elsewhere too. To use that to our advantage, we should check in every call to next
    // if we are within the bounds of our cache or not.
    cache.iterator ++ Iterator.iterate(largestKnown + 2) { _ + 2}
      .filter (isPrime)
  }

  def isPrime(x: Long)(implicit cache: PrimeCache[Long]): Boolean = {
    if (x <= 1) false else if (cache.contains(x)) true else if (cache.last >= x) false else {
      // First check all primes in the cache. When not found, continue expanding the cache until > sqrt x
      if (cache.iterator.exists(divides(x))) false else {
        var counter = cache.last + 2
        var dividerFound = false
        while (!dividerFound && counter * counter <= x) {
          dividerFound = if (isPrime(counter)) {
            cache.add(counter)
            divides(x)(counter)
          } else {
            false
          }
          counter += 2
        }
        !dividerFound
      }
    }
  }

}

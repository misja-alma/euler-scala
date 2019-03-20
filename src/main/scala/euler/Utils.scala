package euler

import scala.annotation.tailrec
import scala.collection.immutable.Set

object Utils {
  def digits(x: BigInt): Seq[Int] = {
    digits(x, BigInt(10))
  }

  def digits(x: Long): Seq[Int] = {
    digits(x, 10)
  }

  def digits[A](x: A, radix: A)(implicit ev: Integral[A]): Seq[Int] = {
    import ev._
    var digits = Seq[Int]()
    var remain = x
    do {
      val digit = remain % radix
      remain /= radix
      digits = digit.toInt +: digits
    } while (remain > zero)
    digits
  }

  def isPermutation(x: Int, y: Int): Boolean = {
    val lengthCheck = if (x > y) {
      y == 0 || (x / y) < 10
    } else {
      x == 0 || (y / x) < 10
    }
    lengthCheck && digits(x).sorted == digits(y).sorted
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

  def pairs[T](set: Set[T]): Set[(T, T)] = set match {
    case s if s.isEmpty => Set()
    case _ => set.tail.map(x => (set.head, x)) ++ pairs(set.tail)
  }

  class PrimeCache[T](implicit ev: Integral[T]) {
    import ev._

    // Initialize already with primes 2 and 3
    private val cache = collection.mutable.SortedSet[T](one + one, one + one + one)

    def contains(x: T): Boolean = cache.contains(x)

    def last: T = cache.lastKey
    
    def iterator: Iterator[T] = cache.iterator
    
    def add(x: T): Unit = cache += x
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

  def fac(x: Int): Int = {
    @tailrec
    def doFac(n: Int, res: Int): Int =
      if (n <= 1) res else doFac(n - 1, n * res)

    doFac(x, 1)
  }
  
  def gcd[T](x: T, y: T)(implicit ev: Integral[T]): T = {
    import ev._

    @tailrec
    def doGcd(big: T, small: T): T = {
      val remainder = big % small
      if (remainder == zero) small else doGcd (small, remainder)
    }

    val px = if (x.signum < 0) -x else x
    val py = if (y.signum < 0) -y else y

    val (big, small) = if (px > py) (px, py) else (py, px)
    doGcd(big, small)
  }

  type FactorCache[T] = collection.mutable.Map[T, Set[T]]

  /**
    *  Returns all distinct prime factors of n.
    *  Note that for prime nrs, this function will return n itself as well. It will also update the caches.
    */
  def distinctPrimeFactors(n: Long)(implicit factorCache: FactorCache[Long], primeCache: PrimeCache[Long]): Set[Long] = {
    val cached = factorCache.get(n)
    cached.getOrElse {
      val factor = primes.dropWhile(p => p * p <= n && n % p != 0).next
      val result = if (factor == n || n % factor != 0) {
        Set(n)
      } else {
        distinctPrimeFactors(factor) ++ distinctPrimeFactors(n / factor)
      }
      factorCache.put(n, result)
      result
    }
  }

  /**
    *  Returns the stream of the nr. of distinct unordered partitions of a set of size n. Includes the set itself.
    *  E.g the nr of n = 4 is 5:
    *  1111, 31, 211, 22 and 4
    *  Note: starts at n = 0
    */
  def unOrderedPartitions[T](implicit ev: Integral[T]): Stream[T] = {
    import ev._

    val partitionCache = new collection.mutable.ArrayBuffer[T]()

    // Assumes that n is always the next un-calculated entry in the partitionCache
    def calcPartitions(n: Int): T = {
      def path(i: Int): T = if (i < 0) zero else partitionCache(i.toInt)

      // This make use of a recurrence relation of the partition function p:
      // p(n) = sum(k>1) -1^(k+1) * (p(n - k(3k - 1)/2) + p(n - k(3k + 1)/2))

      def previousValues(k: Int): T = {
        val i1 = n - (3 * k - 1) * k / 2
        val i2 = n - (3 * k + 1) * k / 2
        path(i1) + path(i2)
      }

      val result = if (n == 0) one else
        Stream
          .from(1)
          .map(previousValues)
          .takeWhile(_ > zero)
          .foldLeft((one, zero)) { case ((k, acc), pr) => (k * -one, acc + k * pr) }
          ._2

      partitionCache.append(result)
      result
    }

    Stream.from(0).map(calcPartitions)
  }
}

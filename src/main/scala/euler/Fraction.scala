package euler

import Utils.gcd

object Fraction {
  def apply[T](numerator: T)(implicit ev: Integral[T]): Fraction[T] = Fraction(numerator, ev.one)

  implicit def numericFraction[T](implicit ev: Integral[T]): Numeric[Fraction[T]] = new Numeric[Fraction[T]]() {

    override def plus(x: Fraction[T], y: Fraction[T]): Fraction[T] = x + y

    override def minus(x: Fraction[T], y: Fraction[T]): Fraction[T] = x - y

    override def times(x: Fraction[T], y: Fraction[T]): Fraction[T] = x * y

    override def negate(x: Fraction[T]): Fraction[T] = -x

    override def fromInt(x: Int): Fraction[T] = Fraction(ev.fromInt(x))

    override def toInt(x: Fraction[T]): Int = ev.toInt(x.toIntegral)

    override def toLong(x: Fraction[T]): Long = ev.toLong(x.toIntegral)

    override def toFloat(x: Fraction[T]): Float = ev.toFloat(x.toIntegral)

    override def toDouble(x: Fraction[T]): Double = ev.toDouble(x.toIntegral)

    override def compare(x: Fraction[T], y: Fraction[T]): Int = x.compare(y)
  }
}

case class Fraction[T](numerator: T, denominator: T)(implicit ev: Integral[T]) extends Ordered[Fraction[T]] {
  import ev._

  def +(f2: Fraction[T]): Fraction[T] =
    Fraction[T](numerator * f2.denominator + f2.numerator * denominator, f2.denominator * denominator)

  def -(f2: Fraction[T]): Fraction[T] =
    Fraction(numerator * f2.denominator - f2.numerator * denominator, f2.denominator * denominator)

  def *(f2: Fraction[T]): Fraction[T] =
    Fraction(numerator * f2.numerator, f2.denominator * denominator)

  def /(f2: Fraction[T]): Fraction[T] =
    Fraction(numerator * f2.denominator, f2.numerator * denominator)

  def simplify: Fraction[T] = {
    val divider = gcd(numerator, denominator)
    Fraction(numerator / divider, denominator / divider)
  }

  def toIntegral: T = numerator / denominator

  def toDouble: Double = numerator.toDouble() / denominator.toDouble()

  // Note that hashcode does not need to be overridden because its contract with equals still holds
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Fraction[T]] && {
      val other = obj.asInstanceOf[Fraction[T]].simplify
      val me = simplify
      other.numerator == me.numerator && other.denominator == me.denominator
    }
  }

  override def compare(that: Fraction[T]): Int = {
    val dif = this - that
    dif.numerator.signum
  }
}


package scas

import scas.math.{Equiv, Ordering}
import scas.structure.{Monoid, Ring, Quotient}
import scala.util.FromDigits

package object arith with
  type BigInteger = java.math.BigInteger

  given BigInteger as Ring[BigInteger] with Ordering[BigInteger] with FromDigits[BigInteger] with
    def fromDigits(digits: String) = new BigInteger(digits)
    def apply(x: BigInteger) = x
    def (x: BigInteger) + (y: BigInteger) = x.add(y)
    def (x: BigInteger) - (y: BigInteger) = x.subtract(y)
    def (x: BigInteger) * (y: BigInteger) = x.multiply(y)
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
    def (x: BigInteger) isZero = x >< 0
    def (x: BigInteger) isOne = x >< 1
    def zero = 0
    def one = 1

  given int2bigInt as Conversion[Int, BigInteger] = java.math.BigInteger.valueOf(_)
  given long2bigInt as Conversion[Long, BigInteger] = java.math.BigInteger.valueOf(_)

  def [T: Monoid](a: T) \: (n: Long): T = a \ n
  def (a: Long) \: (n: Long) = BigInteger(a) \ n

  type Rational = (BigInteger, BigInteger)

  given Rational as Quotient[BigInteger] with Equiv[Rational]
    def apply(a: BigInteger, b: BigInteger) = (a, b)
    def equiv(x: Rational, y: Rational) = x match
      case (a, b) => y match
        case (c, d) => a >< c && b >< d

  given bigInt2rational as Conversion[BigInteger, Rational] = (_, 1)
  given any2rational[U](using Conversion[U, BigInteger]) as Conversion[U, Rational] = (_, 1)

  def (a: Long) /: (b: Long) = Rational(a, b)

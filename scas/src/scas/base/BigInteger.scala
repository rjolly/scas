package scas.base

import scas.structure.commutative.ordered.EuclidianDomain
import scala.util.FromDigits

type BigInteger = java.math.BigInteger

object BigInteger extends EuclidianDomain[BigInteger] with FromDigits[BigInteger] {
  given BigInteger.type = this
  def fromDigits(digits: String) = new BigInteger(digits)
  def apply(str: String) = new BigInteger(str)
  extension (x: BigInteger) {
    def add(y: BigInteger) = x.add(y)
    def subtract(y: BigInteger) = x.subtract(y)
    def multiply(y: BigInteger) = x.multiply(y)
  }
  def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  override def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
  extension (x: BigInteger) {
    override def divide(y: BigInteger) = x.divide(y)
    override def remainder(y: BigInteger) = x.remainder(y)
    def divideAndRemainder(y: BigInteger) = {
      val Array(q, r) = x.divideAndRemainder(y)
      (q, r)
    }
  }
  lazy val characteristic = BigInteger(0)
  extension (x: BigInteger) def isUnit = abs(x).isOne
  extension (x: BigInteger) override def isZero = x.signum == 0
  extension (a: BigInteger) override def pow(b: BigInteger) = a.pow(b.intValue)
  extension (x: BigInteger) override def unary_- = x.negate
  override def abs(x: BigInteger) = x.abs
  extension (x: BigInteger) override def signum = x.signum
  extension (x: BigInteger) def toCode(level: Level) = {
    if (x.bitLength < 32) x.toString
    else if (x.bitLength < 64) x.toString + "l"
    else s"BigInteger($x)"
  }
  override def toString = "BigInteger"
  extension (x: BigInteger) def toMathML = s"<cn>$x</cn>"
  def toMathML = "<integers/>"
  lazy val zero = BigInteger(0)
  lazy val one = BigInteger(1)

  given int2bigInt: (Int => BigInteger) = java.math.BigInteger.valueOf(_)
  given long2bigInt: (Long => BigInteger) = java.math.BigInteger.valueOf(_)

  extension (a: Long) {
    def \ (b: Long): BigInteger = long2bigInt(a) \ b
    def \:(b: Long) = a \ b
  }
}

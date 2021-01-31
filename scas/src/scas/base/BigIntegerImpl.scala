package scas.base

import scas.{BigInteger, int2bigInt}
import scas.structure.commutative.ordered.EuclidianDomain
import scala.util.FromDigits

class BigIntegerImpl extends EuclidianDomain[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = new BigInteger(digits)
  extension (x: BigInteger) {
    def add(y: BigInteger) = x.add(y)
    def subtract(y: BigInteger) = x.subtract(y)
    def multiply(y: BigInteger) = x.multiply(y)
  }
  def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  override def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
  extension (x: BigInteger) {
    override def / (y: BigInteger) = x.divide(y)
    override def % (y: BigInteger) = x.remainder(y)
    def /%(y: BigInteger) = {
      val Array(q, r) = x.divideAndRemainder(y)
      (q, r)
    }
  }
  lazy val characteristic = 0
  extension (x: BigInteger) def isUnit = abs(x).isOne
  extension (x: BigInteger) override def isZero = x.signum == 0
  extension (a: BigInteger) override def \ (b: BigInteger) = a.pow(b.intValue)
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
  lazy val zero = 0
  lazy val one = 1
}

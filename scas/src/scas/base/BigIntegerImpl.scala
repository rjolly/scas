package scas.base

import scas.{BigInteger, int2bigInt}
import scas.structure.commutative.ordered.EuclidianDomain
import scala.util.FromDigits

class BigIntegerImpl extends EuclidianDomain[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = new BigInteger(digits)
  def (x: BigInteger) + (y: BigInteger) = x.add(y)
  def (x: BigInteger) - (y: BigInteger) = x.subtract(y)
  def (x: BigInteger) * (y: BigInteger) = x.multiply(y)
  def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  override def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
  override def (x: BigInteger) / (y: BigInteger) = x.divide(y)
  override def (x: BigInteger) % (y: BigInteger) = x.remainder(y)
  def (x: BigInteger) /%(y: BigInteger) = {
    val Array(q, r) = x.divideAndRemainder(y)
    (q, r)
  }
  lazy val characteristic = 0
  def (x: BigInteger).isUnit = abs(x).isOne
  override def (x: BigInteger).isZero = signum(x) == 0
  override def (a: BigInteger) \ (b: BigInteger) = a.pow(b.intValue)
  override def (x: BigInteger).unary_- = x.negate
  override def abs(x: BigInteger) = x.abs
  override def signum(x: BigInteger) = x.signum
  def (x: BigInteger).toCode(level: Level) = {
    if (x.bitLength < 32) x.toString
    else if (x.bitLength < 64) x.toString + "l"
    else s"BigInteger($x)"
  }
  override def toString = "ZZ"
  def (x: BigInteger).toMathML = s"<cn>$x</cn>"
  def toMathML = "<integers/>"
  lazy val zero = 0
  lazy val one = 1
}

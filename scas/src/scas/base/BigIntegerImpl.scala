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
  def norm(x: BigInteger) = abs(x).shiftLeft(1).add(if (signum(x) < 0) 1 else 0)
  override def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
  override def (x: BigInteger) / (y: BigInteger) = x.divide(y)
  override def (x: BigInteger) % (y: BigInteger) = x.remainder(y)
  def (x: BigInteger) /%(y: BigInteger) = {
    val Array(q, r) = x.divideAndRemainder(y)
    (q, r)
  }
  lazy val characteristic = 0
  def (x: BigInteger).isUnit = abs(x) >< 1
  override def (a: BigInteger) \ (b: BigInteger) = a.pow(b.intValue())
  override def (x: BigInteger).unary_- = x.negate()
  override def abs(x: BigInteger) = x.abs()
  override def signum(x: BigInteger) = x.signum()
  def (x: BigInteger).toCode(level: Level) = {
    if (x.bitLength < 32) x.toString
    else if (x.bitLength < 64) x.toString + "l"
    else s"BigInteger($x)"
  }
  def (x: BigInteger).toMathML = s"<cn>$x</cn>"
  lazy val zero = 0
  lazy val one = 1
}

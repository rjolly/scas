package scas.base

import scas.structure.EuclidianDomain
import scas.{int2bigInteger, long2bigInteger}

object BigInteger extends EuclidianDomain[java.math.BigInteger] {
  def convert(x: java.math.BigInteger) = x
  def apply(s: String) = new java.math.BigInteger(s)
  def apply(l: Long) = l
  def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val r = new java.math.BigInteger(numbits, rnd)
    if (rnd.nextBoolean()) r.negate() else r
  }
  def characteristic = 0
  def isUnit(x: java.math.BigInteger) = abs(x).isOne
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.pow(exp.intValue())
  override def negate(x: java.math.BigInteger) = x.negate()
  override def abs(x: java.math.BigInteger) = x.abs()
  override def signum(x: java.math.BigInteger) = x.signum()
  def norm(x: java.math.BigInteger) = abs(x).shiftLeft(1).add(if (signum(x) < 0) 1 else 0)
  def gcd(x: java.math.BigInteger, y: java.math.BigInteger) = x.gcd(y)
  override def divide(x: java.math.BigInteger, y: java.math.BigInteger) = x.divide(y)
  override def remainder(x: java.math.BigInteger, y: java.math.BigInteger) = x.remainder(y)
  def divideAndRemainder(x: java.math.BigInteger, y: java.math.BigInteger) = {
    val Array(q, r) = x.divideAndRemainder(y)
    (q, r)
  }
  def plus(x: java.math.BigInteger, y: java.math.BigInteger) = x.add(y)
  def minus(x: java.math.BigInteger, y: java.math.BigInteger) = x.subtract(y)
  def times(x: java.math.BigInteger, y: java.math.BigInteger) = x.multiply(y)
  def compare(x: java.math.BigInteger, y: java.math.BigInteger) = x.compareTo(y)
  override def toCode(x: java.math.BigInteger, precedence: Int) = {
    if (x.bitLength < 32) x.toString
    else if (x.bitLength < 64) x.toString + "l"
    else "BigInteger(\"" + x + "\")"
  }
  override def toString = "ZZ"
  def toMathML(x: java.math.BigInteger) = <cn>{x}</cn>
  def toMathML = <integers/>
}

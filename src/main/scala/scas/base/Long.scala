package scas.base

import scas.structure.ordered.EuclidianDomain
import scas.{int2bigInteger, long2bigInteger}

object Long extends EuclidianDomain[Long] {
  def convert(x: Long) = x
  def apply(s: String) = s.toLong
  def apply(l: Long) = l
  def random(numbits: Int)(implicit rnd: java.util.Random) = rnd.nextLong()
  def characteristic = 0
  def isUnit(x: Long) = abs(x).isOne
  override def pow(x: Long, exp: BigInteger) = x.pow(exp.intValue()).longValue
  def norm(x: Long) = java.lang.Long.valueOf((abs(x) << 1) + (if (signum(x) < 0) 1 else 0))
  def gcd(x: Long, y: Long) = x.gcd(y).longValue
  def divideAndRemainder(x: Long, y: Long) = (x / y, x % y)
  def plus(x: Long, y: Long) = x + y
  def minus(x: Long, y: Long) = x - y
  def times(x: Long, y: Long) = x * y
  def compare(x: Long, y: Long) = x compare y
  override def toCode(x: Long, precedence: Int) = {
    if (x == x.toInt) x.toString
    else x.toString + "l"
  }
  override def toString = "ZZ"
  def toMathML(x: Long) = <cn>{x}</cn>
  def toMathML = <integers/>
}

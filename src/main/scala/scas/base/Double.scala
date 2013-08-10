package scas.base

import scas.structure.ordered.Field
import scas.int2bigInteger

object Double extends Field[Double] {
  def convert(x: Double) = x
  def apply(l: Long) = l
  def random(numbits: Int)(implicit rnd: java.util.Random) = rnd.nextDouble()
  def characteristic = 0
  override def pow(x: Double, exp: BigInteger) = Math.pow(x, exp.doubleValue())
  def gcd(x: Double, y: Double) = if (norm(x) < norm(y)) y else x
  def plus(x: Double, y: Double) = x + y
  def minus(x: Double, y: Double) = x - y
  def times(x: Double, y: Double) = x * y
  override def divide(x: Double, y: Double) = x / y
  def inverse(x: Double) = 1 / x
  def compare(x: Double, y: Double) = x compare y
  override def toString = "RR"
  def toMathML(x: Double) = <cn>{x}</cn>
  def toMathML = <reals/>
}

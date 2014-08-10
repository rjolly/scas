package scas.base

import scas.structure.Field
import scas.{int2bigInteger, Variable}
import scas.Implicits.infixAbelianGroupOps

object Function extends Field[Double => Double] {
  implicit val self = this
  def apply(value: Double): Double => Double = { a => value }
  def apply(l: Long) = apply(l.toDouble)
  def signum(x: Double => Double) = if (x.isZero) 0 else 1
  def random(numbits: Int)(implicit rnd: java.util.Random) = { a => rnd.nextDouble() }
  def characteristic = 0
  override def pow(x: Double => Double, exp: BigInteger) = pow(x, { a => exp.doubleValue() })
  def plus(x: Double => Double, y: Double => Double) = { a => x(a) + y(a) }
  def minus(x: Double => Double, y: Double => Double) = { a => x(a) - y(a) }
  def times(x: Double => Double, y: Double => Double) = { a => x(a) * y(a) }
  override def divide(x: Double => Double, y: Double => Double) = { a => x(a) / y(a) }
  def inverse(x: Double => Double) = { a => 1 / x(a) }

  def identity = { a: Double => a }
  def sin(x: Double => Double) = { a: Double => Math.sin(x(a)) }
  def cos(x: Double => Double) = { a: Double => Math.cos(x(a)) }
  def tan(x: Double => Double) = { a: Double => Math.tan(x(a)) }
  def asin(x: Double => Double) = { a: Double => Math.asin(x(a)) }
  def acos(x: Double => Double) = { a: Double => Math.acos(x(a)) }
  def atan(x: Double => Double) = { a: Double => Math.atan(x(a)) }
  def sinh(x: Double => Double) = { a: Double => Math.sinh(x(a)) }
  def cosh(x: Double => Double) = { a: Double => Math.cosh(x(a)) }
  def tanh(x: Double => Double) = { a: Double => Math.tanh(x(a)) }
  def exp(x: Double => Double) = { a: Double => Math.exp(x(a)) }
  def log(x: Double => Double) = { a: Double => Math.log(x(a)) }
  def sqrt(x: Double => Double) = { a: Double => Math.sqrt(x(a)) }
  def pow(x: Double => Double, y: Double => Double) = { a: Double => Math.pow(x(a), y(a)) }

  def equiv(x: Double => Double, y: Double => Double) = x == y
  override def toString = "RR => RR"
  def toMathML(x: Double => Double) = <ci>{x}</ci>
  def toMathML = <mrow><reals/><mo>&#x021A6;</mo><reals/></mrow>
  def function(x: Double => Double, a: Variable) = x
}

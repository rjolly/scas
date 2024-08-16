package scas.scripting

import java.lang.Math
import scas.structure.commutative.Field
import scas.base.BigInteger

object Function extends Field[Double => Double] {
  override def random(numbits: Int)(using rnd: java.util.Random) = { a => rnd.nextDouble() }
  def apply(value: Double): Double => Double = { a => value }
  def fromInt(n: BigInteger) = { a => n.doubleValue() }
  override def zero = { a => 0 }
  override def one = { a => 1 }
  extension (x: Double => Double) {
    def add(y: Double => Double) = { a => x(a) + y(a) }
    def subtract(y: Double => Double) = { a => x(a) - y(a) }
    def multiply(y: Double => Double) = { a => x(a) * y(a) }
    override def divide(y: Double => Double) = { a => x(a) / y(a) }
    override def pow(b: BigInteger) = this.pow(x, { a => b.doubleValue() })
  }
  def inverse(x: Double => Double) = { a => 1 / x(a) }
  def identity = { (a: Double) => a }
  def sin(x: Double => Double) = { (a: Double) => Math.sin(x(a)) }
  def cos(x: Double => Double) = { (a: Double) => Math.cos(x(a)) }
  def tan(x: Double => Double) = { (a: Double) => Math.tan(x(a)) }
  def asin(x: Double => Double) = { (a: Double) => Math.asin(x(a)) }
  def acos(x: Double => Double) = { (a: Double) => Math.acos(x(a)) }
  def atan(x: Double => Double) = { (a: Double) => Math.atan(x(a)) }
  def sinh(x: Double => Double) = { (a: Double) => Math.sinh(x(a)) }
  def cosh(x: Double => Double) = { (a: Double) => Math.cosh(x(a)) }
  def tanh(x: Double => Double) = { (a: Double) => Math.tanh(x(a)) }
  def exp(x: Double => Double) = { (a: Double) => Math.exp(x(a)) }
  def log(x: Double => Double) = { (a: Double) => Math.log(x(a)) }
  def sqrt(x: Double => Double) = { (a: Double) => Math.sqrt(x(a)) }
  def pow(x: Double => Double, y: Double => Double) = { (a: Double) => Math.pow(x(a), y(a)) }
  def equiv(x: Double => Double, y: Double => Double) = x == y
  extension (x: Double => Double) def signum = 0
  override def abs(x: Double => Double) = { a => Math.abs(a) }
  def characteristic = BigInteger("0")
  extension (x: Double => Double) {
    def toCode(level: Level) = x.toString
    def toMathML = s"<ci>$x</ci>"
  }
  override def toString = "Double => Double"
  def toMathML = "<mrow><reals/><mo>&#x021A6;</mo><reals/></mrow>"
}

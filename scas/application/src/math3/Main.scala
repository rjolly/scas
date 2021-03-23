package math3

import java.lang.Math
import scas.structure.Field
import scas.BigInteger
import BigInteger.given
import Matrix.Element

object Double extends Field[Double] {
  given Double.type = this
  extension (x: Double) {
    def add(y: Double) = x + y
    def subtract(y: Double) = x - y
    def multiply(y: Double) = x * y
    override def divide(y: Double) = x / y
  }
  def inverse(x: Double) = 1 / x
  def characteristic = 0
  def equiv(x: Double, y: Double) = x == y
  extension (x: Double) def signum = Math.signum(x).toInt
  def zero = 0
  def one = 1
  extension (x: Double) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???
}

given int2matrix(using Matrix): (Int => Element) = summon[Matrix].one%* _
given double2matrix(using Matrix): (Double => Element) = summon[Matrix].one%* _

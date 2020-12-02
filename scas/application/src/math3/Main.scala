package math3

import java.lang.Math
import scas.structure.Field
import scas.int2bigInt
import Matrix.Element

given Double: Field[Double] with {
  extension (x: Double) {
    def + (y: Double) = x + y
    def - (y: Double) = x - y
    def * (y: Double) = x * y
    override def / (y: Double) = x / y
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

given int2matrix(using Matrix): Conversion[Int, Element] = summon[Matrix].one%* _
given double2matrix(using Matrix): Conversion[Double, Element] = summon[Matrix].one%* _

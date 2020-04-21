package math3

import java.lang.Math
import scas.structure.Field
import scas.int2bigInt
import Matrix.Element

given Double as Field[Double] {
  def (x: Double) + (y: Double) = x + y
  def (x: Double) - (y: Double) = x - y
  def (x: Double) * (y: Double) = x * y
  override def (x: Double) / (y: Double) = x / y
  def inverse(x: Double) = 1 / x
  def characteristic = 0
  def equiv(x: Double, y: Double) = x == y
  def (x: Double).signum = Math.signum(x).toInt
  def zero = 0
  def one = 1
  def (x: Double).toCode(level: Level) = x.toString
  def (x: Double).toMathML = ???
  def toMathML = ???
}

given int2matrix(using Matrix) as Conversion[Int, Element] = summon[Matrix].one%* _
given double2matrix(using Matrix) as Conversion[Double, Element] = summon[Matrix].one%* _

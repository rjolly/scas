package math3

import java.lang.Math
import scas.structure.Field
import scas.prettyprint.Level
import scas.int2bigInt
import Matrix.Element

given Double as Field[Double] with
  def (x: Double) + (y: Double) = x + y
  def (x: Double) - (y: Double) = x - y
  def (x: Double) * (y: Double) = x * y
  override def (x: Double) / (y: Double) = x / y
  def inverse(x: Double) = 1 / x
  def characteristic = 0
  def equiv(x: Double, y: Double) = x == y
  def signum(x: Double) = Math.signum(x).toInt
  def zero = 0
  def one = 1
  def (x: Double).toCode(level: Level) = x.toString
  def (x: Double).toMathML: String = ???

given int2matrix(using Matrix) as Conversion[Int, Element] = summon[Matrix].one:* _
given double2matrix(using Matrix) as Conversion[Double, Element] = summon[Matrix].one:* _

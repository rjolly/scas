package math3

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import scas.structure.{Algebra, Field}
import scas.int2bigInt
import Matrix.Element

class Matrix(size: Int) extends Algebra[Element, Double] with Field[Element] {
  def apply(ds: Double*) = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  def (x: Element) + (y: Element) = x.add(y)
  def (x: Element) - (y: Element) = x.subtract(y)
  def (x: Element) * (y: Element) = x.multiply(y)
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def characteristic = 0
  def equiv(x: Element, y: Element) = x == y
  def signum(x: Element) = if(size > 0) Double.signum(x.getEntry(0, 0)) else 0
  def (x: Double) *%(y: Element) = y%* x
  def (x: Element)%* (y: Double) = x.scalarMultiply(y)
  def zero = MatrixUtils.createRealMatrix(size, size)
  def one = MatrixUtils.createRealIdentityMatrix(size)
  def (x: Element).toCode(level: Level) = x.toString
  def (x: Element).toMathML: String = ???
}

object Matrix {
  type Element = RealMatrix
}

package math3

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import scas.structure.{Algebra, Field}
import scas.int2bigInt
import Matrix.Element

class Matrix(size: Int) extends Algebra[Element, Double] with Field[Element] {
  def apply(ds: Double*) = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  extension (x: Element) {
    def + (y: Element) = x.add(y)
    def - (y: Element) = x.subtract(y)
    def * (y: Element) = x.multiply(y)
  }
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def characteristic = 0
  def equiv(x: Element, y: Element) = x == y
  extension (x: Element) def signum = if(size > 0) x.getEntry(0, 0).signum else 0
  extension (x: Double) def *%(y: Element) = y%* x
  extension (x: Element) def %* (y: Double) = x.scalarMultiply(y)
  def zero = MatrixUtils.createRealMatrix(size, size)
  def one = MatrixUtils.createRealIdentityMatrix(size)
  extension (x: Element) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???
}

object Matrix {
  type Element = RealMatrix
}

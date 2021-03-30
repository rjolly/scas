package math3

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import scas.structure.conversion.{Algebra, Field}
import scas.base.BigInteger
import BigInteger.given
import Matrix.Element
import Double.given

class Matrix(size: Int) extends Algebra[Element, Double] with Field[Element] {
  given Matrix = this
  def apply(ds: Double*): Element = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  extension (x: Element) {
    def add(y: Element) = x.add(y)
    def subtract(y: Element) = x.subtract(y)
    def multiply(y: Element) = x.multiply(y)
  }
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def characteristic = BigInteger(0)
  def equiv(x: Element, y: Element) = x == y
  extension (x: Element) def signum = if(size > 0) x.getEntry(0, 0).signum else 0
  extension (x: Double) def multiplyLeft(y: Element) = y.multiplyRight(x)
  extension (x: Element) def multiplyRight(y: Double) = x.scalarMultiply(y)
  def zero = MatrixUtils.createRealMatrix(size, size)
  def one = MatrixUtils.createRealIdentityMatrix(size)
  extension (x: Element) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???

  given int2matrix(using Matrix): (Int => Element) = summon[Matrix].one%* _
  given double2matrix(using Matrix): (Double => Element) = summon[Matrix].one%* _
}

object Matrix {
  type Element = RealMatrix
}

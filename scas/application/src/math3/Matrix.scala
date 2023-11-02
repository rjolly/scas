package math3

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import scas.structure.impl.{Ring, Field}
import scas.structure.Algebra
import scas.base.BigInteger
import Matrix.Element

trait Matrix extends Algebra[Element, Double] with Field[Element] {
  def size: Int
  def fromInt(n: BigInteger) = one%* Double.fromInt(n)
  def apply(ds: Double*): Element = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  extension (x: Element) {
    def add(y: Element) = x.add(y)
    def subtract(y: Element) = x.subtract(y)
    def multiply(y: Element) = x.multiply(y)
  }
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def equiv(x: Element, y: Element) = x == y
  extension (x: Element) def signum = if(size > 0) x.getEntry(0, 0).sign.toInt else 0
  extension (x: Double) def multiplyLeft(y: Element) = y%* x
  extension (x: Element) def multiplyRight(y: Double) = x.scalarMultiply(y)
  def characteristic = BigInteger("0")
  def zero = MatrixUtils.createRealMatrix(size, size)
  def one = MatrixUtils.createRealIdentityMatrix(size)
  extension (x: Element) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???

  extension (ring: Ring[Double]) def pow(n: Int) = {
    assert (n == size * size)
    this
  }
}

object Matrix {
  type Element = RealMatrix
  def apply(size: Int) = new conversion.Matrix(size)
}

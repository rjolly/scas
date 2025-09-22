package scas.adapter.math3

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import scas.structure.{Algebra, Ring, Field}
import scas.base.BigInteger
import Matrix.Element
import Double.given

trait Matrix extends Algebra[Element, Double] with Field[Element] {
  override given ring: Field[Double] = Double
  def size: Int
  def fromInt(n: BigInteger) = one%* Double.fromInt(n)
  override def zero = MatrixUtils.createRealMatrix(size, size)
  override def one = MatrixUtils.createRealIdentityMatrix(size)
  def apply(ds: Double*): Element = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  extension (x: Element) {
    def add(y: Element) = x.add(y)
    def subtract(y: Element) = x.subtract(y)
    def multiply(y: Element) = x.multiply(y)
  }
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def equiv(x: Element, y: Element) = x == y
  extension (x: Element) def signum = if size > 0 then x.getEntry(0, 0).sign.toInt else 0
  extension (x: Double) def multiplyLeft(y: Element) = y%* x
  extension (x: Element) def multiplyRight(y: Double) = x.scalarMultiply(y)
  def characteristic = BigInteger("0")
  extension (x: Element) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???

  extension (ring: Ring[Double]) def pow(n: Int) = {
    assert (n == size * size)
    this
  }

  given int2matrix: (Int => Element) = one%* _
  given double2matrix: (Double => Element) = one%* _
}

object Matrix {
  type Element = RealMatrix
  def apply(size: Int) = new Conv(size)

  class Conv(val size: Int) extends Matrix with Field.Conv[Element] {
    given instance: Conv = this
  }
}

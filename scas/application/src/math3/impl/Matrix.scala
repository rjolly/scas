package math3.impl

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import scas.structure.impl.{Algebra, Ring, Field}
import scas.base.BigInteger
import math3.Matrix.Element
import math3.Double

trait Matrix extends Algebra[Element, Double] with Field[Element] {
  given instance: Matrix
  val self: Matrix = this
  def size: Int
  def apply(ds: Double*): Element = Array2DRowRealMatrix(ds.grouped(size).map(_.toArray).toArray)
  extension (x: Element) {
    def add(y: Element) = x.add(y)
    def subtract(y: Element) = x.subtract(y)
    def multiply(y: Element) = x.multiply(y)
  }
  def inverse(x: Element) = MatrixUtils.inverse(x)
  def equiv(x: Element, y: Element) = x == y
  extension (x: Element) def signum = if(size > 0) x.getEntry(0, 0).signum else 0
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

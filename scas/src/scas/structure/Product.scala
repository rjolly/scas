package scas.structure

import Product.Element
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.lcm

class Product[R1, R2](using ring1: Ring[R1], ring2: Ring[R2]) extends Ring[Element[R1, R2]] {
  def apply(a: R1, b: R2) = Element(a, b)
  def fromInt(n: BigInteger) = Element(ring1.fromInt(n), ring2.fromInt(n))
  extension (x: Element[R1, R2]) {
    def add(y: Element[R1, R2]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      Element(a + c, b + d)
    }
    def subtract(y: Element[R1, R2]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      Element(a - c, b - d)
    }
    def multiply(y: Element[R1, R2]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      Element(a * c, b * d)
    }
    override def unary_- = {
      val Element(a, b) = x
      Element(-a, -b)
    }
    override def pow(b: BigInteger) = {
      val Element(c, d) = x
      Element(c \ b, d \ b)
    }
    def isUnit = {
      val Element(a, b) = x
      a.isUnit && b.isUnit
    }
    def signum = {
      val Element(a, b) = x
      if (a.signum == 0) b.signum else a.signum
    }
  }
  override def abs(x: Element[R1, R2]) = {
    val Element(a, b) = x
    Element(ring1.abs(a), ring2.abs(b))
  }
  def characteristic = lcm(ring1.characteristic, ring2.characteristic)
  def equiv(x: Element[R1, R2], y: Element[R1, R2]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    a >< c && b >< d
  }
  extension (x: Element[R1, R2]) def toCode(level: Level) = {
    val Element(a, b) = x
    s"Product(${a.show}, ${b.show})"
  }
  override def toString = s"Product($ring1, $ring2)"
  extension (x: Element[R1, R2]) def toMathML = {
    val Element(a, b) = x
    s"<apply><cartesianproduct/>${a.toMathML}${b.toMathML}</apply>"
  }
  def toMathML = s"<apply><cartesianproduct/>${ring1.toMathML}${ring2.toMathML}</apply>"
}

object Product {
  case class Element[R1, R2](a: R1, b: R2)

  def apply[R1, R2, U : Conversion[R1], V : Conversion[R2]](using factory: Product[R1, R2])(a: U, b: V) = factory(~a, ~b)

  def apply[R1, R2](ring1: Ring[R1], ring2: Ring[R2]) = new Conv(using ring1, ring2)

  class Conv[R1 : Ring, R2 : Ring] extends Product[R1, R2] with Ring.Conv[(R1, R2)] {
    given instance: Conv[R1, R2] = this
  }
}

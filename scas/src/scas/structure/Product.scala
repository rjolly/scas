package scas.structure

import scas.base.BigInteger
import Product.Element
import BigInteger.{lcm, given}

class Product[R1, R2](using ring1: Ring[R1], ring2: Ring[R2]) extends Ring[Element[R1, R2]] {
  def apply(a: R1, b: R2): Element[R1, R2] = this(Element(a, b))
  override def apply(x: Element[R1, R2]) = {
    val Element(a, b) = x
    Element(ring1(a), ring2(b))
  }
  extension (x: Element[R1, R2]) def add(y: Element[R1, R2]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    Element(a + c, b + d)
  }
  extension (x: Element[R1, R2]) def subtract(y: Element[R1, R2]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    Element(a - c, b - d)
  }
  extension (x: Element[R1, R2]) def multiply(y: Element[R1, R2]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    Element(a * c, b * d)
  }
  def equiv(x: Element[R1, R2], y: Element[R1, R2]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    a >< c && b >< d
  }
  extension (x: Element[R1, R2]) override def unary_- = {
    val Element(a, b) = x
    Element(-a, -b)
  }
  extension (a: Element[R1, R2]) override def pow(b: BigInteger) = {
    val Element(c, d) = a
    Element(c \ b, d \ b)
  }
  extension (x: Element[R1, R2]) def isUnit = {
    val Element(a, b) = x
    a.isUnit && b.isUnit
  }
  override def abs(x: Element[R1, R2]) = {
    val Element(a, b) = x
    Element(ring1.abs(a), ring2.abs(b))
  }
  extension (x: Element[R1, R2]) def signum = {
    val Element(a, b) = x
    if (a.signum == 0) b.signum else a.signum
  }
  def characteristic = lcm(ring1.characteristic, ring2.characteristic)
  extension (x: Element[R1, R2]) def toCode(level: Level) = {
    val Element(a, b) = x
    s"Product(${a.toCode(Level.Addition)}, ${b.toCode(Level.Addition)})"
  }
  override def toString = s"Product($ring1, $ring2)"
  extension (x: Element[R1, R2]) def toMathML = {
    val Element(a, b) = x
    s"<apply><cartesianproduct/>${a.toMathML}${b.toMathML}</apply>"
  }
  def toMathML = s"<apply><cartesianproduct/>${ring1.toMathML}${ring2.toMathML}</apply>"
  def zero = Element(ring1.zero, ring2.zero)
  def one = Element(ring1.one, ring2.one)
}

object Product {
  case class Element[R1, R2](_1: R1, _2: R2)

  def apply[R1, R2](ring1: Ring[R1], ring2: Ring[R2])(using factory: Product[R1, R2]) = factory
}

package scas.structure

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.lcm

class Product[R1, R2](using ring1: Ring[R1], ring2: Ring[R2]) extends Ring[(R1, R2)] {
  def apply(a: R1, b: R2) = (a, b)
  def fromInt(n: BigInteger) = (ring1.fromInt(n), ring2.fromInt(n))
  extension (x: (R1, R2)) def add(y: (R1, R2)) = {
    val (a, b) = x
    val (c, d) = y
    (a + c, b + d)
  }
  extension (x: (R1, R2)) def subtract(y: (R1, R2)) = {
    val (a, b) = x
    val (c, d) = y
    (a - c, b - d)
  }
  extension (x: (R1, R2)) def multiply(y: (R1, R2)) = {
    val (a, b) = x
    val (c, d) = y
    (a * c, b * d)
  }
  def equiv(x: (R1, R2), y: (R1, R2)) = {
    val (a, b) = x
    val (c, d) = y
    a >< c && b >< d
  }
  extension (x: (R1, R2)) override def unary_- = {
    val (a, b) = x
    (-a, -b)
  }
  extension (a: (R1, R2)) override def pow(b: BigInteger) = {
    val (c, d) = a
    (c \ b, d \ b)
  }
  extension (x: (R1, R2)) def isUnit = {
    val (a, b) = x
    a.isUnit && b.isUnit
  }
  override def abs(x: (R1, R2)) = {
    val (a, b) = x
    (ring1.abs(a), ring2.abs(b))
  }
  extension (x: (R1, R2)) def signum = {
    val (a, b) = x
    if (a.signum == 0) b.signum else a.signum
  }
  def characteristic = lcm(ring1.characteristic, ring2.characteristic)
  extension (x: (R1, R2)) def toCode(level: Level) = {
    val (a, b) = x
    s"Product(${a.show}, ${b.show})"
  }
  override def toString = s"Product($ring1, $ring2)"
  extension (x: (R1, R2)) def toMathML = {
    val (a, b) = x
    s"<apply><cartesianproduct/>${a.toMathML}${b.toMathML}</apply>"
  }
  def toMathML = s"<apply><cartesianproduct/>${ring1.toMathML}${ring2.toMathML}</apply>"
  def zero = (ring1.zero, ring2.zero)
  def one = (ring1.one, ring2.one)
}

object Product {
  def apply[R1, R2, U : Conversion[R1], V : Conversion[R2]](using factory: Product[R1, R2])(a: U, b: V) = factory(~a, ~b)

  def apply[R1, R2](ring1: Ring[R1], ring2: Ring[R2]) = new conversion.Product(using ring1, ring2)
}

package scas.structure.impl

import scas.base.BigInteger
import BigInteger.{lcm, given}

abstract class Product[R1, R2](using ring1: Ring[R1], ring2: Ring[R2]) extends Ring[(R1, R2)] {
  given instance: Product[R1, R2]
  val self: Product[R1, R2] = this
  def apply(n: Long) = (ring1(n), ring2(n))
  def apply(a: R1, b: R2) = (a, b)
  override def convert(x: (R1, R2)) = {
    val (a, b) = x
    (ring1.convert(a), ring2.convert(b))
  }
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
    (c.pow(b), d.pow(b))
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
    s"Product(${a.toCode(Level.Addition)}, ${b.toCode(Level.Addition)})"
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

package scas.structure.commutative.impl

import scas.base.BigInteger
import BigInteger.self.given
import scas.structure.commutative.Quotient.Element

trait Quotient[T](using ring: UniqueFactorizationDomain[T]) extends Field[Element[T]] {
  def apply(n: T) = Element(n, ring.one)
  def apply(n: Long) = this(ring(n))
  def apply(n: T, d: T): Element[T] = this(Element(n, d))
  override def convert(x: Element[T]) = {
    val Element(n, d) = x
    this(ring.convert(n), ring.convert(d))
  }
  def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = if (d.signum == -c.signum) c.negate else c
    Element(n.divide(gcd), d.divide(gcd))
  }
  extension (x: Element[T]) def add(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(b0, d0) = this(b, d)
    this((a.multiply(d0)).add(c.multiply(b0)), b0.multiply(d))
  }
  extension (x: Element[T]) def subtract(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(b0, d0) = this(b, d)
    this((a.multiply(d0)).subtract(c.multiply(b0)), b0.multiply(d))
  }
  extension (x: Element[T]) def multiply(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(a0, d0) = this(a, d)
    val Element(c0, b0) = this(c, b)
    Element(a0.multiply(c0), b0.multiply(d0))
  }
  def equiv(x: Element[T], y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    a >< c && b >< d
  }
  def inverse(x: Element[T]) = {
    val Element(n, d) = x
    Element(d, n)
  }
  override def gcd(x: Element[T], y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    Element(ring.gcd(a, c), ring.lcm(b, d))
  }
  extension (x: Element[T]) override def negate = {
    val Element(n, d) = x
    Element(-n, d)
  }
  extension (a: Element[T]) override def pow(b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else {
    val Element(n, d) = a
    Element(n.pow(b), d.pow(b))
  }
  override def abs(x: Element[T]) = {
    val Element(n, d) = x
    Element(ring.abs(n), d)
  }
  extension (x: Element[T]) def signum = {
    val Element(n, _) = x
    n.signum
  }
  def characteristic = ring.characteristic
  extension (x: Element[T]) def toCode(level: Level) = {
    val Element(n, d) = x
    if (d.isOne) n.toCode(level) else {
      val s = n.toCode(Level.Multiplication) + "/" + d.toCode(Level.Power)
      if (level > Level.Multiplication) fenced(s) else s
    }
  }
  override def toString = s"$ring()"
  extension (x: Element[T]) def toMathML = {
    val Element(n, d) = x
    if (d.isOne) n.toMathML else s"<apply><divide/>${n.toMathML}${d.toMathML}</apply>"
  }
  def toMathML = s"<apply>${ring.toMathML}<list/></apply>"
  def zero = this(ring.zero)
  def one = this(ring.one)

  extension (ring: UniqueFactorizationDomain[T]) def apply() = this
}
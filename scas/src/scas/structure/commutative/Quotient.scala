package scas.structure.commutative

import scas.base.BigInteger
import Quotient.Element
import BigInteger.given

trait Quotient[T: UniqueFactorizationDomain] extends Field[Element[T]] {
  def ring = UniqueFactorizationDomain[T]
  def apply(n: T, d: T): Element[T] = this(Element(n, d))
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = if (d.signum == -c.signum) -c else c
    Element(n / gcd, d / gcd)
  }
  def fromRing(n: T) = Element(n, ring.one)
  extension (x: Element[T]) def add(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(b0, d0) = this(b, d)
    this(a * d0 + c * b0, b0 * d)
  }
  extension (x: Element[T]) def subtract(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(b0, d0) = this(b, d)
    this(a * d0 - c * b0, b0 * d)
  }
  extension (x: Element[T]) def multiply(y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val Element(a0, d0) = this(a, d)
    val Element(c0, b0) = this(c, b)
    Element(a0 * c0, b0 * d0)
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
  extension (x: Element[T]) override def unary_- = {
    val Element(n, d) = x
    Element(-n, d)
  }
  extension (a: Element[T]) override def pow(b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else {
    val Element(n, d) = a
    Element(n \ b, d \ b)
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
  override def toString = s"$ring/$ring"
  extension (x: Element[T]) def toMathML = {
    val Element(n, d) = x
    if (d.isOne) n.toMathML else s"<apply><divide/>${n.toMathML}${d.toMathML}</apply>"
  }
  def toMathML = s"<apply><divide/>${ring.toMathML}${ring.toMathML}</apply>"
  def zero = fromRing(ring.zero)
  def one = fromRing(ring.one)
}

object Quotient {
  case class Element[T](numerator: T, denominator: T)
}

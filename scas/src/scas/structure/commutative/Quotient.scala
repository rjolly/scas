package scas.structure.commutative

import Quotient.Element
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

trait Quotient[T] extends Field[Element[T]] {
  given ring: UniqueFactorizationDomain[T]
  def apply(n: T) = Element(n, ring.one)
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def apply(n: T, d: T): Element[T] = this(Element(n, d))
  def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = if (d.signum == -c.signum) -c else c
    Element(n / gcd, d / gcd)
  }
  extension (x: Element[T]) {
    def add(y: Element[T]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      val Element(b0, d0) = this(b, d)
      this(a * d0 + c * b0, b0 * d)
    }
    def subtract(y: Element[T]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      val Element(b0, d0) = this(b, d)
      this(a * d0 - c * b0, b0 * d)
    }
    def multiply(y: Element[T]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      val Element(a0, d0) = this(a, d)
      val Element(c0, b0) = this(c, b)
      Element(a0 * c0, b0 * d0)
    }
    override def unary_- = {
      val Element(n, d) = x
      Element(-n, d)
    }
    override def pow(b: BigInteger) = if (b.signum < 0) inverse(x) \ -b else {
      val Element(n, d) = x
      Element(n \ b, d \ b)
    }
    def signum = {
      val Element(n, _) = x
      n.signum
    }
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
  override def abs(x: Element[T]) = {
    val Element(n, d) = x
    Element(ring.abs(n), d)
  }
  def characteristic = ring.characteristic
  extension (x: Element[T]) def toCode(level: Level) = {
    import Level.given
    val Element(n, d) = x
    if (d.isOne) n.toCode(level) else {
      val s = n.toCode(Level.Multiplication) + "/" + d.toCode(Level.Power)
      if (level > Level.Multiplication) fenced(s) else s
    }
  }
  override def toString = s"$ring.quotient()"
  extension (x: Element[T]) def toMathML = {
    val Element(n, d) = x
    if (d.isOne) n.toMathML else s"<apply><divide/>${n.toMathML}${d.toMathML}</apply>"
  }

  extension (ring: UniqueFactorizationDomain[T]) def quotient() = this

  given ring2quotient[U: Conversion[T]]: (U => Element[T]) = x => this(~x)
}

object Quotient {
  case class Element[T](numerator: T, denominator: T)
}

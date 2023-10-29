package scas.quotient.impl

import scas.structure.commutative.Quotient.Element
import scas.structure.commutative.impl.{Field, Quotient}
import scas.polynomial.impl.PolynomialOverField

class RationalFunction[T, C, M](using ring: PolynomialOverField[T, C, M]) extends Quotient[T] {
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
  override def toString = ring.toString
  def toMathML = ring.toMathML

  extension (ring: Field[C]) def apply(s: Element[T]*): RationalFunction[T, C, M] = {
    assert (s == generators)
    this
  }
}

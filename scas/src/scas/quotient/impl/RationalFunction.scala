package scas.quotient.impl

import scas.structure.commutative.Quotient.Element
import scas.structure.commutative.impl.Quotient
import scas.polynomial.impl.PolynomialOverField

class RationalFunction[T, C, M](using ring: PolynomialOverField[T, C, M]) extends Quotient[T] {
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)%* ring.lastCoefficient(d)
    Element(n / c, d / c)
  }
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
}

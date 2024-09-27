package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.PolynomialOverField
import scas.util.Conversion

class QuotientOverField[T, C, M](using val ring: PolynomialOverField[T, C, M]) extends Quotient[T, C, M] {
  given Field[C] = ring.ring
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ (c.lastCoefficient / d.lastCoefficient)
    Element(n / gcd, d / gcd)
  }

  extension (ring: PolynomialOverField[T, C, M]) def quotient() = this
}

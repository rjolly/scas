package scas.quotient

import scas.structure.commutative.Quotient.Element
import scas.polynomial.PolynomialOverField

trait RationalFunctionOverField[T, C, M](using ring: PolynomialOverField[T, C, M]) extends Quotient[T, C, M] {
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
}

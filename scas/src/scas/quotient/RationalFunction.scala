package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.PolynomialOverField

trait RationalFunction[T, C, M](using ring: PolynomialOverField[T, C, M]) extends Quotient[T, C, M] {
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new conversion.RationalFunction(using UnivariatePolynomial(ring)(s: _*))
}

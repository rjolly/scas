package scas.quotient

import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends Quotient[Element[C, M], C, M] {
  export ring.coef2poly
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new conversion.RationalFunction(using UnivariatePolynomial(ring)(s: _*))
}

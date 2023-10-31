package scas.quotient.impl

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends Quotient[Element[C, M], C, M] {
  export ring.coef2poly
}

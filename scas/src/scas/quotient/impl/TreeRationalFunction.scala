package scas.quotient.impl

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait TreeRationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends RationalFunction[Element[C, M], C, M] {
  export ring.coef2poly
}

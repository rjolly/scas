package scas.residue.impl

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait TreeAlgebraicNumber[C, M](using ring: UnivariatePolynomial[C, M]) extends AlgebraicNumber[Element[C, M], C, M] {
  export ring.coef2poly
}

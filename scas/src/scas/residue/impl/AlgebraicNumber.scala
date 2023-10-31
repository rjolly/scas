package scas.residue.impl

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait AlgebraicNumber[C, M](using ring: UnivariatePolynomial[C, M]) extends Residue[Element[C, M], C, M] {
  export ring.coef2poly
}

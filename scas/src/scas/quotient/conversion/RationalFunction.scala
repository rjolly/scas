package scas.quotient.conversion

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

class RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends scas.quotient.RationalFunction[C, M] with scas.structure.commutative.conversion.Quotient[Element[C, M]] {
  given instance: RationalFunction[C, M] = this
}

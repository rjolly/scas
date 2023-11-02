package scas.quotient.conversion

import scas.polynomial.tree.UnivariatePolynomial
import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.TreePolynomial

class RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends scas.quotient.RationalFunction[C, M] with Field[Element[TreePolynomial.Element[C, M]]] {
  given instance: RationalFunction[C, M] = this
}

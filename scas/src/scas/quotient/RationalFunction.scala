package scas.quotient

import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

class RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends impl.RationalFunction[C, M] with scas.structure.commutative.Quotient[Element[C, M]] {
  given instance: RationalFunction[C, M] = this
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new RationalFunction(using UnivariatePolynomial(ring)(s: _*))
}
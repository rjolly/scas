package scas.quotient.tree

import scas.structure.commutative.impl.Field
import scas.quotient.impl.TreeRationalFunction
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

class RationalFunction[C, M](using ring: UnivariatePolynomial[C, M]) extends TreeRationalFunction[C, M] with scas.quotient.RationalFunction[Element[C, M], C, M] {
  given instance: RationalFunction[C, M] = this
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new RationalFunction(using UnivariatePolynomial(ring)(s: _*))
}

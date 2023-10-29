package scas.polynomial.tree

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic

class UnivariatePolynomial[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.UnivariatePolynomial[Element[C, M], C, M] {
  given instance: UnivariatePolynomial[C, M] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring, Lexicographic(0)(s: _*))
}

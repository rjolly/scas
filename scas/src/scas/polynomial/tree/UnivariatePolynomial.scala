package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic

class UnivariatePolynomial[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.UnivariatePolynomial[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: UnivariatePolynomial[C, M] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring, Lexicographic[Int](s: _*))
}

package scas.polynomial.tree

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.util.unary_~

class UnivariatePolynomial[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.UnivariatePolynomial[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: UnivariatePolynomial[C, M] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring, new Lexicographic[Int](s.map(~_): _*))
}

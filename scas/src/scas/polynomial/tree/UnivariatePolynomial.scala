package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.impl.Field
import scas.polynomial.conversion.PolynomialOverUFD
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class UnivariatePolynomial[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.UnivariatePolynomial[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: UnivariatePolynomial[C, M] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring, Lexicographic[Int](s: _*))
}

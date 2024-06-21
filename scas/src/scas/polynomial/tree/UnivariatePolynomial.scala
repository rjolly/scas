package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class UnivariatePolynomial[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.UnivariatePolynomial[Element[C, M], C, M] with UniqueFactorizationDomain[Element[C, M]] {
  given ring: Field[C] = summon
  given pp: PowerProduct[M] = summon
  given instance: UnivariatePolynomial[C, M] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring, Lexicographic[Int](s*))
}

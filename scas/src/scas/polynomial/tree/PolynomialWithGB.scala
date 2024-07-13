package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.PolynomialWithGB[Element[C, M], C, M] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
  def newInstance(pp: PowerProduct[M]) = new PolynomialWithGB(using ring, pp)
}

object PolynomialWithGB {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithGB(using ring, Lexicographic[Int](s*))
}

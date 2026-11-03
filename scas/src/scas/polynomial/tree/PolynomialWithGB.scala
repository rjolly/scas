package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.ufd.PolynomialWithGB[Element, C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
  def newInstance = [M] => (pp) => new PolynomialWithGB(using ring, pp)
}

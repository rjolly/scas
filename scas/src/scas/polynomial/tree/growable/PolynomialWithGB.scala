package scas.polynomial.tree.growable

import scas.power.growable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.growable.ufd.PolynomialWithGB[Element, C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
  def newInstance = [M] => (pp) => new scas.polynomial.tree.PolynomialWithGB(using ring, pp)
}

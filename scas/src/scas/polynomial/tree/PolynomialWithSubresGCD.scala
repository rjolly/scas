package scas.polynomial.tree

import scas.power.splittable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends MultivariatePolynomial[C, M] with scas.polynomial.PolynomialWithSubresGCD[Element[C, M], C, M] {
  given instance: MultivariatePolynomial[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithSubresGCD(using ring, pp)
}

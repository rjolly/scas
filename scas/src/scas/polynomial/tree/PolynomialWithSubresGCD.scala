package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C, M](ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends MultivariatePolynomial[C, M](ring, pp) with scas.polynomial.PolynomialWithSubresGCD[Element, C, M] {
  given instance: MultivariatePolynomial[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithSubresGCD(ring, pp)
}

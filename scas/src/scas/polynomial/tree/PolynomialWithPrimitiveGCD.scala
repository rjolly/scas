package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C, M](ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends MultivariatePolynomial[C, M](ring, pp) with scas.polynomial.PolynomialWithPrimitiveGCD[Element, C, M] {
  given instance: MultivariatePolynomial[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithPrimitiveGCD(ring, pp)
}

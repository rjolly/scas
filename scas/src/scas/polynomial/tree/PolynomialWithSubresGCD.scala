package scas.polynomial.tree

import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C : UniqueFactorizationDomain, N : ArrayPowerProduct] extends MultivariatePolynomial[C, N] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using ring, pp)
}

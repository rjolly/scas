package scas.polynomial.tree

import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C : UniqueFactorizationDomain, N : ArrayPowerProduct] extends MultivariatePolynomial[C, N] with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithPrimitiveGCD(using ring, pp)
}

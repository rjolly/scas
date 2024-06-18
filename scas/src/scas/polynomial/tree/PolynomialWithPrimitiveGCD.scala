package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends MultivariatePolynomial[C, M] with scas.polynomial.PolynomialWithPrimitiveGCD[Element[C, M], C, M] {
  given instance: MultivariatePolynomial[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithPrimitiveGCD(using ring, pp)
}

package scas.polynomial.tree

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends MultivariatePolynomial[C, M] with scas.polynomial.impl.PolynomialWithSubresGCD[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: PolynomialWithSubresGCD[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithSubresGCD(using ring, pp)
}

package scas.polynomial.tree

import scas.power.splittable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends MultivariatePolynomial[C, M] with scas.polynomial.PolynomialWithSimpleGCD[Element[C, M], C, M] {
  given instance: PolynomialWithSimpleGCD[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithSimpleGCD(using ring, pp)
}

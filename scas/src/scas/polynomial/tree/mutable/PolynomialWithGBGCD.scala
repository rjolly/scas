package scas.polynomial.tree.mutable

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class PolynomialWithGBGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.ufd.PolynomialWithGB[Element, C, M] with scas.polynomial.mutable.PolynomialWithGB[Element[C, M], C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGBGCD[C, M] = this
  def newInstance = [M] => (pp) => new PolynomialWithGBGCD(using ring, pp)
}

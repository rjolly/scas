package scas.polynomial.tree.growable

import scas.power.POT
import scas.power.growable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGBGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.growable.ufd.PolynomialWithGB[Element, C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGBGCD[C, M] = this
  def newInstance(pp: POT[M]) = new scas.polynomial.tree.PolynomialWithGBGCD(using ring, pp)
}

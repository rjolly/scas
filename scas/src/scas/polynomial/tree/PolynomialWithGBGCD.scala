package scas.polynomial.tree

import scas.power.{PowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGBGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.ufd.PolynomialWithGB[Element[C, M], C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGBGCD[C, M] = this
  def newInstance(pp: POT[M]) = new PolynomialWithGBGCD(using ring, pp)
}

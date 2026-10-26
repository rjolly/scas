package scas.polynomial.list

import scas.power.{PowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.ListPolynomial
import ListPolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, M : PowerProduct] extends ListPolynomial[C, M] with scas.polynomial.ufd.PolynomialWithGB[Element, C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
  def newInstance(pp: POT[M]) = new PolynomialWithGB(using ring, pp)
}

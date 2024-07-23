package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C, M](using val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[M]) extends TreePolynomial[C, M] with scas.polynomial.PolynomialWithGB[Element[C, M], C, M] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
  object Implicits {
    export PolynomialWithGB.this.{instance, coef2poly}
  }
  def newInstance(pp: PowerProduct[M]) = new PolynomialWithGB(using ring, pp)
}

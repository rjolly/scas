package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialOverFieldWithGB[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.ufd.PolynomialOverFieldWithGB[Element, C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialOverFieldWithGB[C, M] = this
  def newInstance = [M] => (pp) => new PolynomialOverFieldWithGB(using ring, pp)
}

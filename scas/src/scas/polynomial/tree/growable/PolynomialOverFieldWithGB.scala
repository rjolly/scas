package scas.polynomial.tree.growable

import scas.power.POT
import scas.power.growable.PowerProduct
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialOverFieldWithGB[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.growable.ufd.PolynomialOverFieldWithGB[Element[C, M], C, M] with UniqueFactorizationDomain.Conv[Element[C, M]] {
  given instance: PolynomialOverFieldWithGB[C, M] = this
  def newInstance(pp: POT[M]) = new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, pp)
}

package scas.polynomial.tree.growable

import scas.power.growable.PowerProduct
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import scas.polynomial.tree.PolynomialWithGB.Element

class PolynomialOverFieldWithGB[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.ufd.growable.PolynomialOverFieldWithGB[Element[C], C, M] with UniqueFactorizationDomain.Conv[Element[C][M]] {
  given instance: PolynomialOverFieldWithGB[C, M] = this
  def newInstance = [M] => (pp) => new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, pp)
}

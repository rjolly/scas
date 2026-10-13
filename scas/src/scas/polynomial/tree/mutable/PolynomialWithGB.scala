package scas.polynomial.tree.mutable

import scas.power.PowerProduct
import scas.structure.Ring
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class PolynomialWithGB[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.mutable.PolynomialWithGB[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
}

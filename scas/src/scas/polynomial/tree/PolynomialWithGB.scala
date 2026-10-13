package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.Ring
import scas.polynomial.TreePolynomial, TreePolynomial.Element

class PolynomialWithGB[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.PolynomialWithGB[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
}

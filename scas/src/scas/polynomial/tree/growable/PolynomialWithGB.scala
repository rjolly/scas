package scas.polynomial.tree.growable

import scas.power.growable.PowerProduct
import scas.structure.Ring
import scas.polynomial.TreePolynomial
import scas.polynomial.growable.PolynomialWithGB
import TreePolynomial.Element

class PolynomialWithGB[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.growable.PolynomialWithGB[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
}

package scas.polynomial.tree.growable

import scas.power.growable.PowerProduct
import scas.structure.Ring
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.growable.Polynomial[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

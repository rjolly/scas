package scas.polynomial.tree.binary

import scas.structure.Ring
import scas.power.compact.binary.PowerProduct
import scas.polynomial.binary.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring](using PowerProduct) extends TreePolynomial[C] with Ring.Conv[Element[C]] {
  given instance: Polynomial[C] = this
}

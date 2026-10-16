package scas.polynomial.tree.binary

import scas.structure.Ring
import scas.power.compact.binary.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring](using PowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.binary.Polynomial[Element[C, Array[Int]], C] with Ring.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
}

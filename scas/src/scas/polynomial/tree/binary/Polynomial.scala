package scas.polynomial.tree.binary

import scas.structure.Ring
import scas.power.compact.BinaryPowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring](using BinaryPowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.binary.Polynomial[Element[C, Array[Int]], C] with Ring.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
}

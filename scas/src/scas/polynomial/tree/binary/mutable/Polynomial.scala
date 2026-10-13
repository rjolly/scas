package scas.polynomial.tree.binary.mutable

import scas.structure.Ring
import scas.power.compact.BinaryPowerProduct
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring](using BinaryPowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.binary.mutable.Polynomial[Element[C, Array[Int]], C] with Ring.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
}

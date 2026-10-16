package scas.polynomial.tree.binary.mutable

import scas.structure.Ring
import scas.power.compact.binary.PowerProduct
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring](using PowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.binary.mutable.Polynomial[Element[C, Array[Int]], C] with Ring.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
}

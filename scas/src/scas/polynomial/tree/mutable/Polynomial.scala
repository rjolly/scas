package scas.polynomial.tree.mutable

import scas.structure.impl.Ring
import scas.power.PowerProduct
import scas.polynomial.impl.TreeMutablePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreeMutablePolynomial[C, M] with scas.polynomial.Polynomial[Element[C, M], C, M] {
  given instance: Polynomial[C, M] = this
}

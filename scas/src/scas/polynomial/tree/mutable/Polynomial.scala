package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreeMutablePolynomial[C, M] with scas.structure.conversion.Ring[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

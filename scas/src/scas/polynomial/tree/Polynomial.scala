package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.conversion.Polynomial[Element[C, M], C, M] {
  given instance: Polynomial[C, M] = this
}

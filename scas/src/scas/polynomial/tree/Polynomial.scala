package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends TreePolynomial[C, M] with scas.structure.conversion.Ring[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

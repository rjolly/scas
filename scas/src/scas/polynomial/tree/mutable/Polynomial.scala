package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends TreeMutablePolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

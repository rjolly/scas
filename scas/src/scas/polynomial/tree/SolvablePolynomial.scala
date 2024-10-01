package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class SolvablePolynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends TreePolynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: SolvablePolynomial[C, M] = this
}

package scas.polynomial.stream.sequential

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.LazyListPolynomial
import LazyListPolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends LazyListPolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

package scas.polynomial.stream.sequential

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.SequentialStreamPolynomial
import scas.polynomial.StreamPolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends SequentialStreamPolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

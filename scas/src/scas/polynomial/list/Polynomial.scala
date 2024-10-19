package scas.polynomial.list

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.ListPolynomial
import ListPolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends ListPolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

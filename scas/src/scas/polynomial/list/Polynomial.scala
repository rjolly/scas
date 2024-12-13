package scas.polynomial.list

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.ListPolynomial
import ListPolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends ListPolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

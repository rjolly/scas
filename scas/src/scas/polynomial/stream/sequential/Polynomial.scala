package scas.polynomial.stream.sequential

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.sequential.StreamPolynomial
import scas.polynomial.StreamPolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends StreamPolynomial[C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}

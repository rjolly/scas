package scas.polynomial.conversion

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.Element

class SolvablePolynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] {
  given SolvablePolynomial[C, M] = this
}

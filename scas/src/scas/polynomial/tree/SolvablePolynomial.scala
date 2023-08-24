package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.Element

class SolvablePolynomial[C : Ring.Impl, M : PowerProduct.Impl] extends Polynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] {
  given instance: SolvablePolynomial[C, M] = this
}

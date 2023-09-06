package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class SolvablePolynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] {
  given instance: SolvablePolynomial[C, M] = this
}

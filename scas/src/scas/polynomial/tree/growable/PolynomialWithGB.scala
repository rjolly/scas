package scas.polynomial.tree.growable

import scas.power.growable.PowerProduct
import scas.structure.Ring
import scas.polynomial.{TreePolynomial, GrowablePolynomial}
import TreePolynomial.Element

class PolynomialWithGB[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with GrowablePolynomial[Element[C, M], C, M] with scas.polynomial.PolynomialWithGB[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: PolynomialWithGB[C, M] = this
}

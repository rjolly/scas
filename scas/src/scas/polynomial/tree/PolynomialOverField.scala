package scas.polynomial.tree

import scas.structure.{Ring, Field}
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialOverField[C : Field, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.PolynomialOverField[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: PolynomialOverField[C, M] = this
}

package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.Element

class WeylAlgebra[C : Ring.Impl, M : PowerProduct.Impl] extends SolvablePolynomial[C, M] with scas.polynomial.WeylAlgebra[Element[C, M], C, M]

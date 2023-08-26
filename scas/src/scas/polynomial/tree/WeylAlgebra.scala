package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.polynomial.TreePolynomial.Element

class WeylAlgebra[C : Ring, M : PowerProduct] extends SolvablePolynomial[C, M] with scas.polynomial.WeylAlgebra[Element[C, M], C, M]

package scas.polynomial.conversion

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.Element

class WeylAlgebra[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.WeylAlgebra[Element[C, M], C, M]

package scas.polynomial.conversion

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.Element

class TreePolynomial[C : Ring, M : PowerProduct] extends scas.polynomial.TreePolynomial[C, M] with Polynomial[Element[C, M], C, M]

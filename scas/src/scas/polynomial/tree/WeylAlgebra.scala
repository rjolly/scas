package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.polynomial.Polynomial
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.base.BigInteger
import BigInteger.given

class WeylAlgebra[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.WeylAlgebra[Element[C, M], C, M] with Polynomial[Element[C, M], C, M] {
  given instance: WeylAlgebra[C, M] = this
}


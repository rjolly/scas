package scas.quotient.impl

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial

class TreeRationalFunction[C : Field, M : PowerProduct] extends RationalFunction(using new UnivariatePolynomial[C, M])

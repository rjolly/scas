package scas.residue.impl

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial

class TreeAlgebraicNumber[C : Field, M : PowerProduct] extends AlgebraicNumber(using new UnivariatePolynomial[C, M])

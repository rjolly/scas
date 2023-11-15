package scas.polynomial

import scas.base.Boolean
import scas.structure.commutative.BooleanUFD

trait BooleanPolynomial[T, M] extends UnivariatePolynomial[T, Boolean, M] with BooleanUFD[T]

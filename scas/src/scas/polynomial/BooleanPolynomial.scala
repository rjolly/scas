package scas.polynomial

import scas.base.Boolean
import scas.structure.BooleanRing

trait BooleanPolynomial[T, M] extends UnivariatePolynomial[T, Boolean, M] with BooleanRing[T]

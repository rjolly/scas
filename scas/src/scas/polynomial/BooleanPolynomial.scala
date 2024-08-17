package scas.polynomial

import scas.base.Boolean

trait BooleanPolynomial[T[Boolean, M], M] extends PolynomialWithSubresGCD[T, Boolean, M] {
  given ring: Boolean.type = Boolean
}

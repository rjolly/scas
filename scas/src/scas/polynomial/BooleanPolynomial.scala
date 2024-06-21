package scas.polynomial

import scas.base.Boolean
import scas.structure.BooleanRing
import scas.structure.commutative.Field

trait BooleanPolynomial[T, M] extends UnivariatePolynomial[T, Boolean, M] with BooleanRing[T] {
  override given ring: Field[Boolean] = Boolean
}

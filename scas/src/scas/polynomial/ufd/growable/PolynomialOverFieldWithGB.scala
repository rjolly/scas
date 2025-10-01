package scas.polynomial.ufd.growable

import scas.structure.commutative.Field

trait PolynomialOverFieldWithGB[T, C, N] extends PolynomialWithGB[T, C, N] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, N] {
  extension (ring: Field[C]) override def apply(s: T*): PolynomialOverFieldWithGB[T, C, N] = {
    same(s*)
    this
  }
}

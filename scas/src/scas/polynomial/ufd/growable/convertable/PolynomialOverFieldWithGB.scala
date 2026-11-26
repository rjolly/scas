package scas.polynomial.ufd.growable.convertable

import scas.structure.commutative.Field

trait PolynomialOverFieldWithGB[T[N], C, N] extends PolynomialWithGB[T, C, N] with scas.polynomial.ufd.growable.PolynomialOverFieldWithGB[T, C, Array[N]] {
  extension (ring: Field[C]) override def apply(s: T[Array[N]]*): PolynomialOverFieldWithGB[T, C, N] = {
    same(s*)
    this
  }
}

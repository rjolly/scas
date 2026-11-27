package scas.polynomial.ufd.convertable

import scas.structure.commutative.Field

trait PolynomialWithModInverse[T, C, N] extends PolynomialOverUFD[T, C, N] with scas.polynomial.ufd.PolynomialWithModInverse[T, C, Array[N]] {
  extension (ring: Field[C]) override def apply(s: T*): PolynomialWithModInverse[T, C, N] = {
    same(s*)
    this
  }
}

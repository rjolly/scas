package scas.polynomial.ufd.growable

import scas.structure.commutative.Field

trait PolynomialWithModInverse[T, C, M] extends PolynomialOverUFD[T, C, M] with scas.polynomial.ufd.PolynomialWithModInverse[T, C, M] {
  extension (ring: Field[C]) override def apply(s: T*): PolynomialWithModInverse[T, C, M] = {
    same(s*)
    this
  }
}

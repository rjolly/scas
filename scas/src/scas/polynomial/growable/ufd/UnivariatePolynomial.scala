package scas.polynomial.growable.ufd

import scas.structure.commutative.Field

trait UnivariatePolynomial[T, C, M] extends PolynomialWithModInverse[T, C, M] with scas.polynomial.ufd.UnivariatePolynomial[T, C, M] {
  extension (ring: Field[C]) override def apply(s: T*): UnivariatePolynomial[T, C, M] = {
    same(s*)
    this
  }
}

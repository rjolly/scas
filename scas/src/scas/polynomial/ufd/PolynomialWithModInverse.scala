package scas.polynomial.ufd

import scas.structure.commutative.Field

trait PolynomialWithModInverse[T, C, M] extends PolynomialOverField[T, C, M] {
  extension (x: T) def modInverse(mods: T*): T
  extension (ring: Field[C]) override def apply(s: T*): PolynomialWithModInverse[T, C, M] = {
    same(s*)
    this
  }
}

package scas.polynomial.ufd.convertable

import scas.polynomial.ConvertablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, N] extends scas.polynomial.ufd.PolynomialOverUFD[T, C, Array[N]] with ConvertablePolynomial[T, C, N] {
  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T*): PolynomialOverUFD[T, C, N] = {
    same(s*)
    this
  }
}

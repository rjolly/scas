package scas.polynomial.growable.ufd

import scas.polynomial.GrowablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends scas.polynomial.ufd.PolynomialOverUFD[T, C, M] with GrowablePolynomial[T, C, M] {
  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T*): PolynomialOverUFD[T, C, M] = {
    same(s*)
    this
  }
}

package scas.polynomial.ufd.growable

import scas.polynomial.GrowablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends GrowablePolynomial[T, C, M] with scas.polynomial.ufd.PolynomialOverUFD[T, C, M]
 {
  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T*) = {
    same(s*)
    this
  }
}

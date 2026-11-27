package scas.polynomial.ufd.growable.convertable

import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialWithGB[T[N], C, N] extends PolynomialOverUFD[T[Array[N]], C, N] with scas.polynomial.ufd.growable.PolynomialWithGB[T, C, Array[N]] {
  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T[Array[N]]*): PolynomialWithGB[T, C, N] = {
    same(s*)
    this
  }
}

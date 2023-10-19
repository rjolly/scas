package scas.polynomial

trait PolynomialOverUFD[T, C, M] extends impl.PolynomialOverUFD[T, C, M] with Polynomial[T, C, M] with scas.structure.commutative.UniqueFactorizationDomain[T]

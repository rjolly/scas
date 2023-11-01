package scas.polynomial.conversion

trait PolynomialOverUFD[T, C, M] extends scas.polynomial.PolynomialOverUFD[T, C, M] with Polynomial[T, C, M] with scas.structure.commutative.UniqueFactorizationDomain[T]

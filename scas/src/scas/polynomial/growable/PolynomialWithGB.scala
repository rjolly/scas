package scas.polynomial.growable

import scas.polynomial.GrowablePolynomial

trait PolynomialWithGB[T, C, M] extends scas.polynomial.PolynomialWithGB[T, C, M] with GrowablePolynomial[T, C, M]

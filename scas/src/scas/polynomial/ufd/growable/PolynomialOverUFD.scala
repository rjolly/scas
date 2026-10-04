package scas.polynomial.ufd.growable

import scas.polynomial.GrowablePolynomial

trait PolynomialOverUFD[T, C, M] extends GrowablePolynomial[T, C, M] with scas.polynomial.ufd.PolynomialOverUFD[T, C, M]

package scas.polynomial.ufd.growable

import scas.structure.commutative.Field

trait PolynomialOverFieldWithGB[T, C, N] extends PolynomialWithGB[T, C, N] with PolynomialWithModInverse[T, C, Array[N]] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, N]

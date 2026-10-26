package scas.polynomial.growable.ufd

trait PolynomialOverFieldWithGB[T[C, M], C, M] extends PolynomialWithGB[T, C, M] with PolynomialWithModInverse[T[C, M], C, M] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, M]

package scas.polynomial.growable.ufd

trait PolynomialOverFieldWithGB[T[M], C, M] extends PolynomialWithGB[T, C, M] with PolynomialWithModInverse[T[M], C, M] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, M]

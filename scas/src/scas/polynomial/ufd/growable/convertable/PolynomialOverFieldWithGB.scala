package scas.polynomial.ufd.growable.convertable

trait PolynomialOverFieldWithGB[T[N], C, N] extends PolynomialWithGB[T, C, N] with PolynomialWithModInverse[T[Array[N]], C, N] with scas.polynomial.ufd.growable.PolynomialOverFieldWithGB[T, C, Array[N]]

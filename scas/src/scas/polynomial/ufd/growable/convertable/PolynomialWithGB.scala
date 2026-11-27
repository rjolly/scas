package scas.polynomial.ufd.growable.convertable

trait PolynomialWithGB[T[N], C, N] extends PolynomialOverUFD[T[Array[N]], C, N] with scas.polynomial.ufd.growable.PolynomialWithGB[T, C, Array[N]]

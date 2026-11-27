package scas.polynomial.ufd

trait MultivariatePolynomialOverField[T[C], C, N] extends PolynomialWithSubresGCD[T, C, N] with PolynomialOverField[T[C], C, Array[N]]

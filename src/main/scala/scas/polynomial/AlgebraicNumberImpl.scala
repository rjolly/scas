package scas.polynomial

class AlgebraicNumberImpl[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N]

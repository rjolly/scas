package scas.polynomial.residue

import scas.polynomial.{PolynomialOverUFD, UnivariatePolynomial}

class AlgebraicNumberImpl[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](implicit val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N]

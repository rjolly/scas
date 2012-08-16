package scas.polynomial.residue

import scas.polynomial.{PolynomialOverUFD, UnivariatePolynomial}

class AlgebraicNumber[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumberLike[R, C, N]

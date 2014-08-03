package scas.residue

import scas.polynomial.ufd.{PolynomialOverUFD, UnivariatePolynomial}

class AlgebraicNumberImpl[R <: PolynomialOverUFD.Element[R, C, N], C, N](val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N]

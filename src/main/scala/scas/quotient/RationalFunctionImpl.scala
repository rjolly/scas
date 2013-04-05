package scas.quotient

import scas.polynomial.PolynomialOverUFD

class RationalFunctionImpl[R <: PolynomialOverUFD.Element[R, C, N], C, N](implicit val ring: PolynomialOverUFD[R, C, N]) extends RationalFunction[R, C, N]

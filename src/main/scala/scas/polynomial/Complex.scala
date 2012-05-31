package scas.polynomial

import scas.int2bigInteger

class Complex[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N] {
  val I = generators(0)
  update(one + pow(I, 2))
}

package scas.polynomial.residue

import scas.polynomial.{PolynomialOverUFD, UnivariatePolynomial}
import scas.int2bigInteger
import Residue.Element

class Complex[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N] {
  update(one + pow(generator(0), 2))
  def sqrt(x: Element[R, C, N]) = {
    assert (x >< -one)
    generator(0)
  }
}

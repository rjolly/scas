package scas.polynomial

import PolynomialOverUFD.Element

trait PolynomialWithMonicGCD[T[C, N] <: Element[T[C, N], C, N], C, @specialized(Int, Long) N] extends MultivariatePolynomial[T, C, N] with PolynomialOverField[T[C, N], C, N] {
  def gcd1(x: T[C, N], y: T[C, N]) = if (y.isZero) monic(x) else gcd1(y, monic(reduce(x, y)))
}

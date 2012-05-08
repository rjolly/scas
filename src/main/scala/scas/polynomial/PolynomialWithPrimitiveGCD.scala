package scas.polynomial

import PolynomialOverUFD.Element

trait PolynomialWithPrimitiveGCD[T[C, N] <: Element[T[C, N], C, N], C, N] extends MultivariatePolynomial[T, C, N] {
  def gcd1(x: T[C, N], y: T[C, N]) = if (y.isZero) x else gcd1(y, primitivePart(reduce(x, y)))
}

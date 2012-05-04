package scas.polynomial

import scas.Implicits.infixUFDOps
import PolynomialOverUFD.Element

trait PolynomialWithPrimitiveGCD[T[C, N] <: Element[T[C, N], C, N], C, N] extends MultivariatePolynomial[T, C, N] {
  override def reduce(x: T[C, N], y: T[C, N]) = remainder1(x, y)
  def gcd1(x: T[C, N], y: T[C, N]) = if (y.isZero) x else gcd1(y, primitivePart(reduce(x, y)))
}

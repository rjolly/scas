package scas.polynomial.ufd

import scas.long2bigInteger
import scas.Implicits.infixUFDOps
import PolynomialOverUFD.Element

trait PolynomialWithSubresGCD[T[C, N] <: Element[T[C, N], C, N], C, N] extends MultivariatePolynomial[T, C, N] {
  def gcd1(x: T[C, N], y: T[C, N]) = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  def gcd(x: T[C, N], y: T[C, N], beta: C, phi: C): T[C, N] = if (y.isZero) x else if (x.isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, divide(reduce(x, y), beta), headCoefficient(x) * ring.pow(phi, d), if (d == 0) phi else if (d == 1) headCoefficient(y) else ring.pow(headCoefficient(y), d) / ring.pow(phi, d - 1))
  }
}

package scas.polynomial.ufd

import scala.annotation.tailrec

trait PolynomialWithSimpleGCD[T[C], C, N] extends MultivariatePolynomial[T, C, N] {
  @tailrec final def gcd1(x: T[C], y: T[C]): T[C] = if y.isZero then x else gcd1(y, x.reduce(y))
}

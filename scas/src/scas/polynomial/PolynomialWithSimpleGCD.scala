package scas.polynomial

import scala.annotation.tailrec

trait PolynomialWithSimpleGCD[T[C, M], C, M] extends MultivariatePolynomial[T, C, M] {
  @tailrec final def gcd1(x: T[C, M], y: T[C, M]): T[C, M] = if (y.isZero) x else gcd1(y, x.reduce(y))
}

package scas.polynomial.ufd

import scala.annotation.tailrec

trait PolynomialWithSimpleGCD[T[C, M], C, N] extends MultivariatePolynomial[T, C, N] {
  @tailrec final def gcd1(x: T[C, Array[N]], y: T[C, Array[N]]): T[C, Array[N]] = if y.isZero then x else gcd1(y, x.reduce(y))
}

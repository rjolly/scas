package scas.polynomial.ufd

import scala.annotation.tailrec

trait PolynomialWithPrimitiveGCD[T[C], C, N] extends MultivariatePolynomial[T, C, N] {
  @tailrec final def gcd1(x: T[C], y: T[C]): T[C] = if y.isZero then primitivePart(x) else gcd1(y, primitivePart(x.reduce(y)))
}

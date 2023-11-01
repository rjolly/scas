package scas.polynomial

import scala.annotation.tailrec

trait PolynomialWithPrimitiveGCD[T, C, M] extends PolynomialOverUFD[T, C, M] {
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) primitivePart(x) else gcd1(y, primitivePart(x.reduce(y)))
}

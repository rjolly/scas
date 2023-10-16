package scas.polynomial.ufd

import scala.annotation.tailrec

trait PolynomialWithSimpleGCD[T, C, M] extends PolynomialOverUFD[T, C, M] {
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) x else gcd1(y, x.reduce(y))
}

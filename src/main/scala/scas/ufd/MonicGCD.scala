package scas.ufd

import scala.annotation.tailrec
import scas.polynomial.{PolynomialOverUFD, PolynomialOverField}
import PolynomialOverUFD.Element

trait MonicGCD[T <: Element[T, C, N], C, N] { this: PolynomialOverField[T, C, N] =>
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(reduce(x, y)))
}

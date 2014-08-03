package scas.polynomial.ufd

import scala.annotation.tailrec
import PolynomialOverUFD.Element

trait PolynomialWithPrimitiveGCD[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] {
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) primitivePart(x) else gcd1(y, primitivePart(reduce(x, y)))
}

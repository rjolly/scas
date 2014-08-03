package scas.quotient

import scas.polynomial.ufd.{PolynomialOverUFD, PolynomialOverField}
import RationalFunction.Element

class RationalFunctionOverField[R <: PolynomialOverUFD.Element[R, C, N], C, N](val ring: PolynomialOverField[R, C, N]) extends RationalFunction[R, C, N] {
  override def apply(n: R, d: R) = {
    val c = ring(ring.lastCoefficient(d))
    new Element[R, C, N](n / c, d / c)(this)
  }
}

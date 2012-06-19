package scas.polynomial.quotient

import scas.polynomial.{PolynomialOverUFD, PolynomialOverField}
import RationalFunction.Element

class RationalFunctionOverField[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](override implicit val ring: PolynomialOverField[R, C, N]) extends RationalFunction[R, C, N] {
  override def apply(n: R, d: R) = {
    val c = ring(ring.lastCoefficient(d))
    new Element[R, C, N](n / c, d / c)(this)
  }
}

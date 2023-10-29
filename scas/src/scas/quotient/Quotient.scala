package scas.quotient

import scas.polynomial.impl.PolynomialOverField

class Quotient[T, C, M](using PolynomialOverField[T, C, M]) extends RationalFunction[T, C, M] {
  val ring = summon[PolynomialOverField[T, C, M]]
  given instance: Quotient[T, C, M] = this
}

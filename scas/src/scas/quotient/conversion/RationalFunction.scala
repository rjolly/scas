package scas.quotient.conversion

import scas.polynomial.PolynomialOverField

class RationalFunction[T, C, M](using PolynomialOverField[T, C, M]) extends scas.quotient.RationalFunction[T, C, M] with Quotient[T, C, M] {
  given instance: RationalFunction[T, C, M] = this
}

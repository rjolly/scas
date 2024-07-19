package scas.quotient.conversion

import scas.polynomial.PolynomialOverField

class RationalFunctionOverField[T, C, M](using val ring: PolynomialOverField[T, C, M]) extends scas.quotient.RationalFunctionOverField[T, C, M] with Quotient[T, C, M] {
  given instance: RationalFunctionOverField[T, C, M] = this
}

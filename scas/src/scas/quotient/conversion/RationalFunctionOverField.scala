package scas.quotient.conversion

import scas.polynomial.PolynomialOverField
import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element

class RationalFunctionOverField[T, C, M](using val ring: PolynomialOverField[T, C, M]) extends scas.quotient.RationalFunctionOverField[T, C, M] with Field[Element[T]] {
  given instance: RationalFunctionOverField[T, C, M] = this
}

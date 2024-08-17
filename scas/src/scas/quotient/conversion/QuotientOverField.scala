package scas.quotient.conversion

import scas.polynomial.PolynomialOverField
import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element

class QuotientOverField[T, C, M](using val ring: PolynomialOverField[T, C, M]) extends scas.quotient.QuotientOverField[T, C, M] with Field[Element[T]] {
  given instance: QuotientOverField[T, C, M] = this
}

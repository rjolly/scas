package scas.quotient

import scas.polynomial.PolynomialOverField

class Quotient[T, C, M](using PolynomialOverField[T, C, M]) extends impl.Quotient[T, C, M] with scas.structure.commutative.conversion.Quotient[T] {
  given instance: Quotient[T, C, M] = this
}

package scas.quotient

import scas.polynomial.impl.PolynomialOverField

class Quotient[T, C, M](using PolynomialOverField[T, C, M]) extends impl.RationalFunction[T, C, M] with scas.structure.commutative.Quotient[T] {
  given instance: Quotient[T, C, M] = this
}

package scas.quotient.conversion

import scas.polynomial.PolynomialOverField

class RationalFunction[T, C, M](using PolynomialOverField[T, C, M]) extends Quotient[T, C, M] with scas.quotient.RationalFunction[T, C, M]

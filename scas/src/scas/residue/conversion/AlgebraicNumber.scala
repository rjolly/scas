package scas.residue.conversion

import scas.polynomial.PolynomialOverField

class AlgebraicNumber[T, C, M](using PolynomialOverField[T, C, M]) extends Residue[T, C, M] {
  given instance: AlgebraicNumber[T, C, M] = this
}

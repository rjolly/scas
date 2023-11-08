package scas.residue.conversion

import scas.polynomial.PolynomialOverField
import scas.base.BigInteger

class GaloisField[T, M](using PolynomialOverField[T, Int, M]) extends Residue[T, Int, M] {
  given instance: GaloisField[T, M] = this
}

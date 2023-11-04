package scas.residue.conversion

import scas.polynomial.PolynomialOverField
import scas.base.BigInteger

class GaloisField[T, M](using PolynomialOverField[T, BigInteger, M]) extends Residue[T, BigInteger, M] {
  given instance: GaloisField[T, M] = this
}

package scas.residue.conversion

import scala.reflect.ClassTag
import scas.polynomial.PolynomialOverField
import scas.base.BigInteger

class GaloisField[T, M](using PolynomialOverField[T, Int, M])(using ClassTag[T]) extends Residue[T, Int, M] {
  given instance: GaloisField[T, M] = this
}

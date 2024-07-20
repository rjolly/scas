package scas.residue.conversion

import scala.reflect.ClassTag
import scas.polynomial.PolynomialOverField

class AlgebraicNumber[T, C, M](using val ring: PolynomialOverField[T, C, M])(using ClassTag[T]) extends Residue[T, C, M] {
  given instance: AlgebraicNumber[T, C, M] = this
}

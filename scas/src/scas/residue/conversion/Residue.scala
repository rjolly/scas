package scas.residue.conversion

import scala.reflect.ClassTag
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.conversion.Field

class Residue[T : ClassTag, C, M](using PolynomialOverField[T, C, M])(s: T*) extends scas.residue.Residue[T, C, M] with Field[T] {
  given instance: Residue[T, C, M] = this
  update(s*)
}

package scas.residue.conversion

import scala.reflect.ClassTag
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.conversion.Field

class ResidueOverField[T : ClassTag, C, M](using val ring: PolynomialOverField[T, C, M])(s: T*) extends scas.residue.ResidueOverField[T, C, M] with Field[T] {
  given instance: ResidueOverField[T, C, M] = this
  update(s*)
}

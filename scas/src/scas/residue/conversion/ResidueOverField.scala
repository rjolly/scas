package scas.residue.conversion

import scala.reflect.ClassTag
import scas.polynomial.PolynomialOverFieldWithGB
import scas.structure.commutative.conversion.Field

class ResidueOverField[T : ClassTag, C, N](using val ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) extends scas.residue.ResidueOverField[T, C, N] with Field[T] {
  given instance: ResidueOverField[T, C, N] = this
  update(s*)
}

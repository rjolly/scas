package scas.residue.growable

import scala.reflect.ClassTag
import scala.compiletime.deferred
import scas.structure.commutative.Field
import scas.polynomial.ufd.growable.PolynomialOverFieldWithGB

trait ResidueOverField[T, C, N] extends Residue[T, C, N] with scas.residue.ResidueOverField[T, C, N] {
  given ring: PolynomialOverFieldWithGB[T, C, N] = deferred

  extension (ring: PolynomialOverFieldWithGB[T, C, N]) def apply(s: T*) = {
    same(s*)
    this
  }
}

object ResidueOverField {
  class Conv[T : ClassTag, C, N](using PolynomialOverFieldWithGB[T, C, N])(s: T*) extends ResidueOverField[T, C, N] with Field.Conv[T] {
    given instance: Conv[T, C, N] = this
    update(s*)
  }
}

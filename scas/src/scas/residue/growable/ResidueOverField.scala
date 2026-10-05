package scas.residue.growable

import scala.reflect.ClassTag
import scala.compiletime.deferred
import scas.structure.commutative.Field
import scas.polynomial.ufd.growable.PolynomialWithModInverse

trait ResidueOverField[T, C, M] extends Residue[T, C, M] with scas.residue.ResidueOverField[T, C, M] {
  given ring: PolynomialWithModInverse[T, C, M] = deferred

  extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
}

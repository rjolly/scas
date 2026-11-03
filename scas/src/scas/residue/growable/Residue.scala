package scas.residue.growable

import scala.compiletime.deferred
import scas.variable.Variable
import scas.polynomial.growable.Polynomial

trait Residue[T, C, M] extends scas.residue.Residue[T, C, M] {
  given ring: Polynomial[T, C, M] = deferred
  def extend(variables: Variable*): Unit = {
    ring.extend(variables*)
  }

  extension (ring: Polynomial[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
}

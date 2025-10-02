package scas.residue.growable

import scala.compiletime.deferred
import scas.variable.Variable
import scas.polynomial.ufd.growable.PolynomialWithGB

trait Residue[T, C, N] extends scas.residue.Residue[T, C, N] {
  given ring: PolynomialWithGB[T, C, N] = deferred
  def extend(variables: Variable*): Unit = {
    ring.extend(variables*)
    mods = mods.map(_.convert)
  }

  extension (ring: PolynomialWithGB[T, C, N]) def apply(s: T*) = {
    same(s*)
    this
  }
}

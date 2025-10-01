package scas.residue.growable

import scala.compiletime.deferred
import scas.variable.Variable
import scas.polynomial.ufd.growable.{PolynomialWithGB, PolynomialOverFieldWithGB}

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

object Residue {
  def apply[T : ClassTag, C, N](ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) = new ResidueOverField.Conv(using ring)(s*)
}

package scas.residue

import scas.structure.commutative.Field
import scas.polynomial.impl.PolynomialOverField
import scas.util.{Conversion, unary_~}

trait Residue[T, C, M] extends impl.Residue[T, C, M] with Field[T] {
  given instance: Residue[T, C, M]
  def sqrt[U: Conversion[T]](x: U) = super.sqrt(~x)
}

object Residue {
  def apply[T, C, M](using ring: PolynomialOverField[T, C, M])(mod: T) = new Residue[T, C, M] {
    given instance: Residue[T, C, M] = this
    update(mod)
  }
}

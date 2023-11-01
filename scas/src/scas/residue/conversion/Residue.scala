package scas.residue.conversion

import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}

trait Residue[T, C, M] extends scas.residue.Residue[T, C, M] with Field[T] {
  given instance: Residue[T, C, M]
  def sqrt[U: Conversion[T]](x: U) = super.sqrt(~x)
}

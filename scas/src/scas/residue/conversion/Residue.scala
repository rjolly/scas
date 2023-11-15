package scas.residue.conversion

import scas.structure.commutative.conversion.Field
import scas.util.{Conversion, unary_~}

trait Residue[T, C, M] extends scas.residue.Residue[T, C, M] with Field[T] {
  def sqrt[U: Conversion[T]](x: U) = super.sqrt(~x)
}

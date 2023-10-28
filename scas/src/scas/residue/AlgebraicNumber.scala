package scas.residue

import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}

trait AlgebraicNumber[T, C, M] extends impl.AlgebraicNumber[T, C, M] with Field[T] {
  def sqrt[U: Conversion[T]](x: U) = super.sqrt(~x)
}

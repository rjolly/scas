package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait Structure[T] extends scas.structure.Structure[T] with scas.math.conversion.Equiv[T] {
  def convert[U: Conversion[T]](x: U) = super.convert(~x)
}

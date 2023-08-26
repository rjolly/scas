package scas.structure

import scas.math.Equiv
import scas.util.{Conversion, unary_~}

trait Structure[T] extends impl.Structure[T] with Equiv[T] {
  def convert[U: Conversion[T]](x: U) = super.convert(~x)
}

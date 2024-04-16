package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension[U: Conversion[T]] (x: U) {
    inline def + [V: Conversion[T]](y: V) = (~x).add(~y)
    inline def - [V: Conversion[T]](y: V) = (~x).subtract(~y)
  }
  def abs[U: Conversion[T]](x: U) = super.abs(~x)
}

package scas.structure

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends impl.AbelianGroup[T] with Structure[T] {
  extension (x: T) {
    inline def + [U: Conversion[T]](y: U) = x.add(~y)
    inline def - [U: Conversion[T]](y: U) = x.subtract(~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def + [V: Conversion[T]](y: V) = (~x).add(~y)
    inline def - [V: Conversion[T]](y: V) = (~x).subtract(~y)
  }
  def abs[U: Conversion[T]](x: U) = super.abs(~x)
}

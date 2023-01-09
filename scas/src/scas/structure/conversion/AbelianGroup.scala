package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension[U: Conversion[T]] (x: U) {
    def + [V: Conversion[T]](y: V) = (~x).add(~y)
    def - [V: Conversion[T]](y: V) = (~x).subtract(~y)
    def unary_- = super.unary_-(~x)
  }
  extension (x: T) {
    def + [U: Conversion[T]](y: U) = x.add(~y)
    def - [U: Conversion[T]](y: U) = x.subtract(~y)
  }
  def abs[U: Conversion[T]](x: U) = super.abs(~x)
}

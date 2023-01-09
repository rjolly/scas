package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    def <=[V: Conversion[T]](y: V) = lteq(~x, ~y)
    def >=[V: Conversion[T]](y: V) = gteq(~x, ~y)
    def < [V: Conversion[T]](y: V) = lt(~x, ~y)
    def > [V: Conversion[T]](y: V) = gt(~x, ~y)
  }
  extension (x: T) {
    def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    def < [U: Conversion[T]](y: U) = lt(x, ~y)
    def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
}

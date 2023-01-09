package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    def <=(y: T) = lteq(~x, y)
    def >=(y: T) = gteq(~x, y)
    def < (y: T) = lt(~x, y)
    def > (y: T) = gt(~x, y)
  }
  extension (x: T) {
    def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    def < [U: Conversion[T]](y: U) = lt(x, ~y)
    def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
}

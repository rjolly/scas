package scas.math

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scala.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    inline def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    inline def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    def < [U: Conversion[T]](y: U) = lt(x, ~y)
    def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
}

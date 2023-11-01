package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    inline def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    inline def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    inline def < [U: Conversion[T]](y: U) = lt(x, ~y)
    inline def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
  extension [U: Conversion[T]](x: U) {
    inline def <=[V: Conversion[T]](y: V) = lteq(~x, ~y)
    inline def >=[V: Conversion[T]](y: V) = gteq(~x, ~y)
    inline def < [V: Conversion[T]](y: V) = lt(~x, ~y)
    inline def > [V: Conversion[T]](y: V) = gt(~x, ~y)
  }
}

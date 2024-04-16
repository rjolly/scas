package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension [U: Conversion[T]](x: U) {
    inline def <=[V: Conversion[T]](y: V) = lteq(~x, ~y)
    inline def >=[V: Conversion[T]](y: V) = gteq(~x, ~y)
    inline def < [V: Conversion[T]](y: V) = lt(~x, ~y)
    inline def > [V: Conversion[T]](y: V) = gt(~x, ~y)
  }
}

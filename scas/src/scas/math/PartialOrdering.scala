package scas.math

import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scala.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    inline def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    inline def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    inline def < [U: Conversion[T]](y: U) = lt(x, ~y)
    inline def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
}

object PartialOrdering {
  trait Ops[T] extends Equiv.Ops[T] { this: PartialOrdering[T] =>
    extension [U: Conversion[T]](x: U) {
      inline def <=(y: T) = lteq(~x, y)
      inline def >=(y: T) = gteq(~x, y)
      inline def < (y: T) = lt(~x, y)
      inline def > (y: T) = gt(~x, y)
    }
  }
}

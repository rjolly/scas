package scas.math

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scala.math.Equiv[T] {
  extension (x: T) {
    inline def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    inline def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
}

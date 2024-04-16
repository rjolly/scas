package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scas.math.Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    inline def ><[V: Conversion[T]](y: V) = equiv(~x, ~y)
    inline def <>[V: Conversion[T]](y: V) = !equiv(~x, ~y)
  }
}

package scas.math

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends impl.Equiv[T] {
  extension (x: T) {
    inline def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    inline def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def ><[V: Conversion[T]](y: V) = equiv(~x, ~y)
    inline def <>[V: Conversion[T]](y: V) = !equiv(~x, ~y)
  }
}

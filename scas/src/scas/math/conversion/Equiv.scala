package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scas.math.Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    inline def ><(y: T) = equiv(~x, y)
    inline def <>(y: T) = !equiv(~x, y)
  }
  extension (x: T) {
    inline def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    inline def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
}

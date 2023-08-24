package scas.math

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends Equiv.Impl[T] {
  extension (x: T) {
    inline def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    inline def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def ><(y: T) = equiv(~x, y)
    inline def <>(y: T) = !equiv(~x, y)
  }
}

object Equiv {
  trait Impl[T] extends scala.math.Equiv[T] {
    extension (x: T) {
      inline def ><(y: T) = equiv(x, y)
      inline def <>(y: T) = !equiv(x, y)
    }
  }
}

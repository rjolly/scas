package scas.math

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scala.math.Equiv[T] {
  extension (x: T) {
    inline def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    inline def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
}

object Equiv {
  trait Ops[T](using self: Equiv[T]) {
    import self.equiv
    extension[U: Conversion[T]] (x: U) {
      inline def ><(y: T) = equiv(~x, y)
      inline def <>(y: T) = !equiv(~x, y)
    }
  }
}

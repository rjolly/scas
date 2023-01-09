package scas.math.conversion

import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scas.math.Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    def ><[V: Conversion[T]](y: V) = equiv(~x, ~y)
    def <>[V: Conversion[T]](y: V) = !equiv(~x, ~y)
  }
  extension (x: T) {
    def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
}

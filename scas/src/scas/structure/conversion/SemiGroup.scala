package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends scas.structure.SemiGroup[T] with Structure[T] {
  extension[U: Conversion[T]] (x: U) inline def * (y: T) = (~x).multiply(y)
  extension (x: T) {
    inline def * [U: Conversion[T]](y: U) = x.multiply(~y)
  }
}

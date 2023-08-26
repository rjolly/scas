package scas.structure

import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends impl.SemiGroup[T] with Structure[T] {
  extension (x: T) inline def * [U: Conversion[T]](y: U) = x.multiply(~y)
  extension[U: Conversion[T]] (x: U) inline def * (y: T) = (~x).multiply(y)
}

package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends scas.structure.SemiGroup[T] with Structure[T] {
  extension[U: Conversion[T]] (x: U) inline def * [V: Conversion[T]](y: V) = (~x).multiply(~y)
}

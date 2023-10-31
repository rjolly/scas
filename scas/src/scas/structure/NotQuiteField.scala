package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends impl.NotQuiteField[T] with Ring[T] {
  extension (x: T) inline def / [U: Conversion[T]](y: U) = x.divide(~y)
  extension[U: Conversion[T]] (x: U) inline def / [V: Conversion[T]](y: V) = (~x).divide(~y)
}

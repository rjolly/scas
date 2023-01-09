package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends scas.structure.NotQuiteField[T] with Ring[T] {
  extension[U: Conversion[T]] (x: U) def / [V: Conversion[T]](y: V) = (~x).divide(~y)
  extension (x: T) {
    def / [U: Conversion[T]](y: U) = x.divide(~y)
  }
}

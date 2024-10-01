package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) {
    def divide(y: T): T
    inline def / [U: Conversion[T]](y: U) = x.divide(~y)
  }
}

object NotQuiteField {
  trait Conv[T] extends NotQuiteField[T] with Ring.Conv[T] {
    extension[U: Conversion[T]] (x: U) inline def / [V: Conversion[T]](y: V) = (~x).divide(~y)
  }
}

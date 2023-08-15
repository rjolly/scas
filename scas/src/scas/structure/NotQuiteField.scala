package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) {
    def divide(y: T): T
    inline def / [U: Conversion[T]](y: U) = x.divide(~y)
  }
}

object NotQuiteField {
  trait Ops[T: NotQuiteField] extends Ring.Ops[T] {
    extension[U: Conversion[T]] (x: U) inline def / (y: T) = (~x).divide(y)
  }
}

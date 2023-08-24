package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends NotQuiteField.Impl[T] with Ring[T] {
  extension (x: T) inline def / [U: Conversion[T]](y: U) = x.divide(~y)
  extension[U: Conversion[T]] (x: U) inline def / (y: T) = (~x).divide(y)
}

object NotQuiteField {
  trait Impl[T] extends Ring.Impl[T] {
    extension (x: T) {
      def divide(y: T): T
      inline def / (y: T) = x.divide(y)
    }
  }
}

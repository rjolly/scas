package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) {
    def divide(y: T): T
    inline def / [U: Conversion[T]](y: U) = x.divide(~y)
  }
}

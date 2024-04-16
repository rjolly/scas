package scas.structure

import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends Structure[T] {
  extension (x: T) {
    def multiply(y: T): T
    inline def * [U: Conversion[T]](y: U) = x.multiply(~y)
  }
}

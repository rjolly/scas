package scas.structure

import scas.math.Equiv
import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends Structure[T] {
  extension (x: T) {
    def multiply(y: T): T
    inline def * [U: Conversion[T]](y: U) = x.multiply(~y)
  }
}

object SemiGroup {
  trait Conv[T] extends SemiGroup[T] with Equiv.Conv[T] {
    extension[U: Conversion[T]] (x: U) inline def * [V: Conversion[T]](y: V) = (~x).multiply(~y)
  }
}

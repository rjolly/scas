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
  trait Ops[T] extends Equiv.Ops[T] { this: SemiGroup[T] =>
    extension[U: Conversion[T]] (x: U) inline def * (y: T) = (~x).multiply(y)
  }
}

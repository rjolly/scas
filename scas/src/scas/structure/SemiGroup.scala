package scas.structure

import scas.util.{Conversion, unary_~}

trait SemiGroup[T] extends SemiGroup.Impl[T] with Structure[T] {
  extension (x: T) inline def * [U: Conversion[T]](y: U) = x.multiply(~y)
  extension[U: Conversion[T]] (x: U) inline def * (y: T) = (~x).multiply(y)
}

object SemiGroup {
  trait Impl[T] extends Structure.Impl[T] {
    extension (x: T) {
      def multiply(y: T): T
      inline def * (y: T) = x.multiply(y)
    }
  }
}

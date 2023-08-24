package scas.structure

import scas.util.{Conversion, unary_~}

trait Module[T, R : Ring.Impl] extends Module.Impl[T, R] with AbelianGroup[T] {
  extension[U: Conversion[R]] (x: U) def *%(y: T) = (~x).multiplyLeft(y)
  extension (x: T) def %* [U: Conversion[R]](y: U) = x.multiplyRight(~y)
}

object Module {
  trait Impl[T, R : Ring.Impl] extends AbelianGroup.Impl[T] {
    extension (x: R) def multiplyLeft(y: T): T
    extension (x: T) def multiplyRight(y: R): T
    extension (x: R) def *%(y: T) = x.multiplyLeft(y)
    extension (x: T) def %* (y: R) = x.multiplyRight(y)

    extension (ring: Ring.Impl[R]) def pow(n: Int): Impl[T, R]
  }
}

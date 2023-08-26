package scas.structure

import scas.util.{Conversion, unary_~}

trait Module[T, R : impl.Ring] extends impl.Module[T, R] with AbelianGroup[T] {
  extension[U: Conversion[R]] (x: U) def *%(y: T) = (~x).multiplyLeft(y)
  extension (x: T) def %* [U: Conversion[R]](y: U) = x.multiplyRight(~y)
}

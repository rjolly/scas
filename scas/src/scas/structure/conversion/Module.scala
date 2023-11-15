package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait Module[T, R] extends scas.structure.Module[T, R] with AbelianGroup[T] {
  extension[U: Conversion[R]] (x: U) def *%(y: T) = (~x).multiplyLeft(y)
  extension (x: T) def %* [U: Conversion[R]](y: U) = x.multiplyRight(~y)
}

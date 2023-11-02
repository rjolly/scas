package scas.structure.conversion

import scas.util.{Conversion, unary_~}
import scas.structure.AbelianGroup

trait Module[T, R : scas.structure.impl.Ring] extends scas.structure.Module[T, R] with AbelianGroup[T] {
  extension[U: Conversion[R]] (x: U) def *%(y: T) = (~x).multiplyLeft(y)
  extension (x: T) def %* [U: Conversion[R]](y: U) = x.multiplyRight(~y)
}

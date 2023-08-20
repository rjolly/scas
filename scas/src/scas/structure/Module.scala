package scas.structure

import scas.util.{Conversion, unary_~}

trait Module[T, R: Ring] extends AbelianGroup[T] {
  extension (x: R) def multiplyLeft(y: T): T
  extension (x: T) def multiplyRight(y: R): T
  extension[U: Conversion[R]] (x: U) def *%(y: T) = (~x).multiplyLeft(y)
  extension (x: T) def %* [U: Conversion[R]](y: U) = x.multiplyRight(~y)

  extension (ring: Ring[R]) def pow(n: Int): Module[T, R]
}

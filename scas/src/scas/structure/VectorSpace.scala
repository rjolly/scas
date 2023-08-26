package scas.structure

import scas.util.{Conversion, unary_~}

trait VectorSpace[T, R : impl.Field] extends impl.VectorSpace[T, R] with Module[T, R] {
  extension (x: T) def %/ [U: Conversion[R]](y: U) = x.divideRight(~y)
}

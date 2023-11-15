package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait VectorSpace[T, R] extends scas.structure.VectorSpace[T, R] with Module[T, R] {
  extension (x: T) def %/ [U: Conversion[R]](y: U) = x.divideRight(~y)
}

package scas.structure

import scas.util.{Conversion, unary_~}

trait VectorSpace[T, R] extends Module[T, R] {
  given ring: Field[R]
  extension (x: T) def divideRight(y: R) = x%* ring.inverse(y)
  extension (x: T) def %/ [U: Conversion[R]](y: U) = x.divideRight(~y)
}

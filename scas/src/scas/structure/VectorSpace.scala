package scas.structure

import scas.util.{Conversion, unary_~}

trait VectorSpace[T, R : Field.Impl] extends VectorSpace.Impl[T, R] with Module[T, R] {
  extension (x: T) def %/ [U: Conversion[R]](y: U) = x.divideRight(~y)
}

object VectorSpace {
  trait Impl[T, R : Field.Impl] extends Module.Impl[T, R] {
    extension (x: T) def divideRight(y: R) = x%* Field[R].inverse(y)
    extension (x: T) def %/ (y: R) = x.divideRight(y)
  }
}

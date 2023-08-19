package scas.structure

import scas.util.{Conversion, unary_~}

trait VectorSpace[T, R: Field] extends Module[T, R] {
  extension (x: T) def divideRight(y: R) = x%* Field[R].inverse(y)
  extension (x: T) def %/ [U: Conversion[R]](y: U) = x.divideRight(~y)
}

object VectorSpace {
  trait Ops[T, R] extends Module.Ops[T, R] { this: VectorSpace[T, R] =>
  }
}

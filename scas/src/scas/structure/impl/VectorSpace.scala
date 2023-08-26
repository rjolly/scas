package scas.structure.impl

trait VectorSpace[T, R : Field] extends Module[T, R] {
  extension (x: T) def divideRight(y: R) = x%* Field[R].inverse(y)
  extension (x: T) def %/ (y: R) = x.divideRight(y)
}

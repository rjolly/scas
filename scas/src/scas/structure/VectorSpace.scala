package scas.structure

import scas.structure.Field

abstract class VectorSpace[T, R: Field] extends Module[T, R] {
  extension[U] (x: U)(using Conversion[U, T]) def %/ (y: R): T = (x: T)%/ y
  extension (x: T) def %/ (y: R) = x%* Field[R].inverse(y)
}

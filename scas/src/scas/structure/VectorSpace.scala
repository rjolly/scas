package scas.structure

import scas.structure.Field

abstract class VectorSpace[T, R: Field] extends Module[T, R] {
  extension (x: T) def %/ (y: R) = x%* Field[R].inverse(y)
}

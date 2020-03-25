package scas.structure

import scas.structure.Field

abstract class VectorSpace[T, R: Field] extends Module[T, R] {
  def (x: T)%/ (y: R) = x%* Field[R].inverse(y)
}

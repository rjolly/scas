package scas.structure

import scas.structure.Field

trait VectorSpace[T, R: Field] extends Module[T, R] with
  def (x: T):/ (y: R) = Field[R].inverse(y):*:x

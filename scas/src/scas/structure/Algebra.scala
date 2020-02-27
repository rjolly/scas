package scas.structure

import scas.structure.Field

trait Algebra[T, R: Field] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

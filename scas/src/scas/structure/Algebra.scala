package scas.structure

import impl.Field

trait Algebra[T, R : Field] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

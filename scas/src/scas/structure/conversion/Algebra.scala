package scas.structure.conversion

trait Algebra[T, R] extends scas.structure.Algebra[T, R] with AlgebraOverRing[T, R] with VectorSpace[T, R]

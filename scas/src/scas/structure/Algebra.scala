package scas.structure

trait Algebra[T, R : impl.Field] extends impl.Algebra[T, R] with AlgebraOverRing[T, R] with VectorSpace[T, R]

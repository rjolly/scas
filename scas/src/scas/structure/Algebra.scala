package scas.structure

trait Algebra[T, R : Field] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

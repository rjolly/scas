package scas.structure

trait Algebra[T, R] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

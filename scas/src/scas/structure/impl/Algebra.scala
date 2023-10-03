package scas.structure.impl

trait Algebra[T, R : Field] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

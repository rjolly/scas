package scas.structure

trait Algebra[T <: (Int => R), R] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

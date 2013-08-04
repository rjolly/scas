package scas.structure

import AlgebraOverRing.Element

trait Algebra[T <: Element[T, R], R] extends AlgebraOverRing[T, R] with VectorSpace[T, R]

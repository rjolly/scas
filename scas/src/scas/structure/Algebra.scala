package scas.structure

trait Algebra[T, R: Field] extends VectorSpace[T, R] with SemiGroup[T]

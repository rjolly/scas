package scas.structure

trait Algebra[T, R : impl.Field] extends impl.Algebra[T, R] with VectorSpace[T, R] with SemiGroup[T]

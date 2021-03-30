package scas.structure.conversion

trait Algebra[T, R: scas.structure.Field] extends scas.structure.Algebra[T, R] with VectorSpace[T, R] with SemiGroup[T]

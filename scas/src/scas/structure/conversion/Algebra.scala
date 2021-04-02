package scas.structure.conversion

abstract class Algebra[T, R: scas.structure.Field] extends VectorSpace[T, R] with SemiGroup[T] with scas.structure.Algebra[T, R]

package scas.structure

import scas.structure.Field

abstract class Algebra[T, R: Field] extends VectorSpace[T, R] with SemiGroup[T]

package scas.structure

trait Algebra[T, R : Field.Impl] extends Algebra.Impl[T, R] with VectorSpace[T, R] with SemiGroup[T]

object Algebra {
  trait Impl[T, R : Field.Impl] extends VectorSpace.Impl[T, R] with SemiGroup.Impl[T]
}

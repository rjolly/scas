package scas.structure

trait Algebra[T, R: Field] extends VectorSpace[T, R] with SemiGroup[T]

object Algebra {
  trait Ops[T, R](using Algebra[T, R]) extends Module.Ops[T, R] with SemiGroup.Ops[T]
}

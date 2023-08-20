package scas.structure

trait Algebra[T, R: Field] extends VectorSpace[T, R] with SemiGroup[T]

object Algebra {
  trait Ops[T, R] extends AbelianGroup.Ops[T] with SemiGroup.Ops[T] { this: Algebra[T, R] =>
  }
}

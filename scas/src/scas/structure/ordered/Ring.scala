package scas.structure.ordered

trait Ring[T] extends scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

object Ring {
  trait Ops[T] extends scas.structure.Ring.Ops[T] with AbelianGroup.Ops[T] with Monoid.Ops[T] { this: Ring[T] =>
  }
}

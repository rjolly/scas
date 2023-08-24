package scas.structure.ordered

trait Ring[T] extends Ring.Impl[T] with scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

object Ring {
  trait Impl[T] extends scas.structure.Ring.Impl[T] with AbelianGroup.Impl[T] with Monoid.Impl[T]
}

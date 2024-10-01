package scas.structure.ordered

trait Ring[T] extends scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

object Ring {
  trait Conv[T] extends Ring[T] with scas.structure.Ring.Conv[T] with AbelianGroup.Conv[T] with Monoid.Conv[T]
}

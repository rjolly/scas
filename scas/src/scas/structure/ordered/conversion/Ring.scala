package scas.structure.ordered.conversion

trait Ring[T] extends scas.structure.ordered.Ring[T] with scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

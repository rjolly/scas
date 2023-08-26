package scas.structure.ordered

trait Ring[T] extends impl.Ring[T] with scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

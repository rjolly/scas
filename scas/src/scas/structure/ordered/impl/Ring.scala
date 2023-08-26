package scas.structure.ordered.impl

trait Ring[T] extends scas.structure.impl.Ring[T] with AbelianGroup[T] with Monoid[T]

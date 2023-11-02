package scas.structure.ordered

trait Ring[T] extends scas.structure.impl.Ring[T] with AbelianGroup[T] with Monoid[T]

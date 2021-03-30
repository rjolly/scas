package scas.structure.conversion

trait Ring[T] extends scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T]

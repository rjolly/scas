package scas.structure.ordered

trait Monoid[@specialized(Int, Long) T] extends SemiGroup[T] with scas.structure.Monoid[T]

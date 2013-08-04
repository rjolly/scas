package scas.structure.ordered

trait Monoid[@specialized(Int, Long, Double) T] extends SemiGroup[T] with scas.structure.Monoid[T]

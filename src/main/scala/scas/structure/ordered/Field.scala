package scas.structure.ordered

trait Field[@specialized(Int, Long) T] extends EuclidianDomain[T] with scas.structure.Field[T]

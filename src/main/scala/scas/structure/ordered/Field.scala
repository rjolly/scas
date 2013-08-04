package scas.structure.ordered

trait Field[@specialized(Int, Long, Double) T] extends EuclidianDomain[T] with scas.structure.Field[T]

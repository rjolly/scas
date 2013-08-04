package scas.structure.ordered

trait Structure[@specialized(Int, Long, Double) T] extends scas.structure.Structure[T] with scas.math.Ordering[T]

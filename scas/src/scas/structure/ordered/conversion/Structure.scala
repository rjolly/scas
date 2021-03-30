package scas.structure.ordered.conversion

trait Structure[T] extends scas.structure.ordered.Structure[T] with scas.structure.conversion.Structure[T] with scas.math.conversion.Ordering[T]

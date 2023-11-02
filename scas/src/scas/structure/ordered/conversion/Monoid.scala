package scas.structure.ordered.conversion

trait Monoid[T] extends scas.structure.ordered.Monoid[T] with scas.structure.conversion.Monoid[T] with Structure[T]

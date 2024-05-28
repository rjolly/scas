package scas.structure.ordered.conversion

import scas.math.conversion.PartialOrdering

trait Monoid[T] extends scas.structure.ordered.Monoid[T] with scas.structure.conversion.Monoid[T] with PartialOrdering[T]

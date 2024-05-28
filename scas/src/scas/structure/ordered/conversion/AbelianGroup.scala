package scas.structure.ordered.conversion

import scas.math.conversion.PartialOrdering

trait AbelianGroup[T] extends scas.structure.ordered.AbelianGroup[T] with scas.structure.conversion.AbelianGroup[T] with PartialOrdering[T]

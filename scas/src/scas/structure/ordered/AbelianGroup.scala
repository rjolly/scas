package scas.structure.ordered

import scas.math.PartialOrdering

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension (x: T) def signum = if (x < zero) -1 else if (x > zero) 1 else 0
}

object AbelianGroup {
  trait Conv[T] extends AbelianGroup[T] with scas.structure.AbelianGroup.Conv[T] with PartialOrdering.Conv[T]
}

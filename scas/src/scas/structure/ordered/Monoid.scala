package scas.structure.ordered

import scas.math.PartialOrdering

trait Monoid[T] extends scas.structure.Monoid[T] with Structure[T]

object Monoid {
  trait Conv[T] extends Monoid[T] with scas.structure.Monoid.Conv[T] with PartialOrdering.Conv[T]
}

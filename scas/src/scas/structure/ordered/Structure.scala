package scas.structure.ordered

trait Structure[T] extends scas.structure.Structure[T] with scas.math.Ordering[T]

object Structure {
  trait Ops[T: Structure] extends scas.math.PartialOrdering.Ops[T]
}

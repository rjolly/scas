package scas.structure.ordered

trait Structure[T] extends scas.structure.Structure[T] with scas.math.Ordering[T]

object Structure {
  trait Ops[T] extends scas.structure.Structure.Ops[T] with scas.math.PartialOrdering.Ops[T] { this: Structure[T] =>
  }
}

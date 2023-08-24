package scas.structure.ordered

trait Structure[T] extends Structure.Impl[T] with scas.structure.Structure[T] with scas.math.Ordering[T]

object Structure {
  trait Impl[T] extends scas.structure.Structure.Impl[T] with scas.math.Ordering.Impl[T]
}

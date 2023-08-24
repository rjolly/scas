package scas.structure.commutative.ordered

trait Field[T] extends Field.Impl[T] with scas.structure.commutative.Field[T] with EuclidianDomain[T]

object Field {
  trait Impl[T] extends scas.structure.commutative.Field.Impl[T] with EuclidianDomain.Impl[T]
}

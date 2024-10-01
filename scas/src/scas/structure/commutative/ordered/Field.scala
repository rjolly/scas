package scas.structure.commutative.ordered

trait Field[T] extends scas.structure.commutative.Field[T] with EuclidianDomain[T]

object Field {
  trait Conv[T] extends Field[T] with scas.structure.commutative.Field.Conv[T] with UniqueFactorizationDomain.Conv[T]
}

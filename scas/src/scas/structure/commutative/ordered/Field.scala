package scas.structure.commutative.ordered

trait Field[T] extends scas.structure.commutative.Field[T] with EuclidianDomain[T]

object Field {
  trait Ops[T: Field] extends scas.structure.commutative.Field.Ops[T] with UniqueFactorizationDomain.Ops[T]
}

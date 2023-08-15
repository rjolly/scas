package scas.structure.commutative.ordered

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.ordered.Ring[T]

object UniqueFactorizationDomain {
  trait Ops[T: UniqueFactorizationDomain] extends scas.structure.commutative.UniqueFactorizationDomain.Ops[T] with scas.structure.ordered.Ring.Ops[T]
}

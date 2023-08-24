package scas.structure.commutative.ordered

trait UniqueFactorizationDomain[T] extends UniqueFactorizationDomain.Impl[T] with scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.ordered.Ring[T]

object UniqueFactorizationDomain {
  trait Impl[T] extends scas.structure.commutative.UniqueFactorizationDomain.Impl[T] with scas.structure.ordered.Ring.Impl[T]
}

package scas.structure.commutative.ordered

trait EuclidianDomain[T] extends EuclidianDomain.Impl[T] with scas.structure.commutative.EuclidianDomain[T] with UniqueFactorizationDomain[T]

object EuclidianDomain {
  trait Impl[T] extends scas.structure.commutative.EuclidianDomain.Impl[T] with UniqueFactorizationDomain.Impl[T]
}

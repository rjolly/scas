package scas.structure.commutative.ordered

import scas.structure.ordered.Ring

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with Ring[T]

object UniqueFactorizationDomain {
  trait Conv[T] extends UniqueFactorizationDomain[T] with scas.structure.commutative.UniqueFactorizationDomain.Conv[T] with Ring.Conv[T]
}

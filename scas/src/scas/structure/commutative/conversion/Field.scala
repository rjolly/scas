package scas.structure.commutative.conversion

trait Field[T] extends scas.structure.commutative.Field[T] with scas.structure.conversion.Field[T] with UniqueFactorizationDomain[T]

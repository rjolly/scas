package scas.structure.commutative.ordered.conversion

abstract class Residue[T: scas.structure.commutative.ordered.UniqueFactorizationDomain] extends scas.structure.commutative.conversion.Residue[T] with scas.structure.commutative.ordered.Residue[T] with UniqueFactorizationDomain[T]

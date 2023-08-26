package scas.structure.commutative.ordered

trait Residue[T : impl.UniqueFactorizationDomain] extends impl.Residue[T] with scas.structure.commutative.Residue[T] with UniqueFactorizationDomain[T]

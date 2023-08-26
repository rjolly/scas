package scas.structure.commutative

trait Residue[T : impl.UniqueFactorizationDomain] extends impl.Residue[T] with UniqueFactorizationDomain[T]

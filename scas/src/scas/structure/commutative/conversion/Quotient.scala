package scas.structure.commutative.conversion

trait Quotient[T: scas.structure.commutative.UniqueFactorizationDomain] extends scas.structure.commutative.Quotient[T] with Field[scas.structure.commutative.Quotient.Element[T]]

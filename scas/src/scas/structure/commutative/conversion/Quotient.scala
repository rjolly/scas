package scas.structure.commutative.conversion

import scas.structure.commutative.Quotient.Element

trait Quotient[T: scas.structure.commutative.UniqueFactorizationDomain] extends scas.structure.commutative.Quotient[T] with Field[Element[T]]

package scas.structure.commutative.ordered

import scas.structure.commutative.Quotient.Element

trait Quotient[T : impl.UniqueFactorizationDomain] extends impl.Quotient[T] with scas.structure.commutative.Quotient[T] with Field[Element[T]]

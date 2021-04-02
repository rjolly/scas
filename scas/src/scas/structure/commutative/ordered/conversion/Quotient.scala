package scas.structure.commutative.ordered.conversion

import scas.structure.commutative.Quotient.Element

abstract class Quotient[T: scas.structure.commutative.ordered.UniqueFactorizationDomain] extends scas.structure.commutative.conversion.Quotient[T] with scas.structure.commutative.ordered.Quotient[T] with Field[Element[T]]

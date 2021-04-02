package scas.structure.commutative.conversion

import Quotient.Element

abstract class Quotient[T: scas.structure.commutative.UniqueFactorizationDomain] extends scas.structure.commutative.Quotient[T] with Field[Element[T]]

val Quotient = scas.structure.commutative.Quotient

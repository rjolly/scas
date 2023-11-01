package scas.structure.commutative.ordered.conversion

import scas.structure.commutative.ordered.Field
import scas.structure.commutative.Quotient.Element

trait Quotient[T : scas.structure.commutative.ordered.impl.UniqueFactorizationDomain] extends scas.structure.commutative.ordered.Quotient[T] with scas.structure.commutative.conversion.Quotient[T] with Field[Element[T]]

package scas.quotient.conversion

import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element

trait Quotient[T, C, M] extends scas.quotient.Quotient[T, C, M] with Field[Element[T]]

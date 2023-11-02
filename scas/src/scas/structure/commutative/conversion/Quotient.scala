package scas.structure.commutative.conversion

import scas.structure.commutative.Quotient.Element
import scas.util.{Conversion, unary_~}

trait Quotient[T] extends scas.structure.commutative.Quotient[T] with Field[Element[T]] {
  given ring2quotient[U: Conversion[T]]: (U => Element[T]) = x => this(~x)
}

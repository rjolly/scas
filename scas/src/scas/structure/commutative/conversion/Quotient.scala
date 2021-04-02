package scas.structure.commutative.conversion

import scas.structure.commutative.Quotient.Element

abstract class Quotient[T: scas.structure.commutative.UniqueFactorizationDomain] extends scas.structure.commutative.Quotient[T] with Field[Element[T]] {
  given ring2quotient[U](using c: U => T): (U => Element[T]) = x => this(c(x))
}

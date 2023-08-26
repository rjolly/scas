package scas.structure.commutative

import scas.util.{Conversion, unary_~}
import Quotient.Element

trait Quotient[T : impl.UniqueFactorizationDomain] extends impl.Quotient[T] with Field[Element[T]] {
  given ring2quotient[U: Conversion[T]]: (U => Element[T]) = x => this(~x)
}

object Quotient {
  case class Element[T](numerator: T, denominator: T)
}

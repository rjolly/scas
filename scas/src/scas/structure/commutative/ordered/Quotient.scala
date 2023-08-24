package scas.structure.commutative.ordered

import scas.structure.commutative.Quotient.Element

trait Quotient[T : UniqueFactorizationDomain.Impl] extends Quotient.Impl[T] with scas.structure.commutative.Quotient[T] with Field[Element[T]]

object Quotient {
  trait Impl[T](using ring: UniqueFactorizationDomain.Impl[T]) extends scas.structure.commutative.Quotient.Impl[T] with Field.Impl[Element[T]] {
    def compare(x: Element[T], y: Element[T]) = {
      val Element(a, b) = x
      val Element(c, d) = y
      ring.compare(a.multiply(d), c.multiply(b))
    }
    extension (x: Element[T]) override def signum = super.signum(x)
  }
}

package scas.structure.commutative.ordered.impl

import scas.structure.commutative.Quotient.Element

trait Quotient[T](using ring: UniqueFactorizationDomain[T]) extends scas.structure.commutative.impl.Quotient[T] with Field[Element[T]] {
  def compare(x: Element[T], y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    ring.compare(a.multiply(d), c.multiply(b))
  }
  extension (x: Element[T]) override def signum = super.signum(x)
}

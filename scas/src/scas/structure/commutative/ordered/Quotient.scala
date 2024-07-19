package scas.structure.commutative.ordered

import scas.structure.commutative.Quotient.Element

trait Quotient[T] extends scas.structure.commutative.Quotient[T] with Field[Element[T]] {
  given ring: UniqueFactorizationDomain[T]
  def compare(x: Element[T], y: Element[T]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    ring.compare(a * d, c * b)
  }
  extension (x: Element[T]) override def signum = super.signum(x)
}

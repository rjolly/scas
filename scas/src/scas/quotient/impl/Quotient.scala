package scas.quotient.impl

import scas.polynomial.impl.PolynomialOverField
import scas.structure.commutative.Quotient.Element

trait Quotient[T, C, M](using ring: PolynomialOverField[T, C, M]) extends scas.structure.commutative.impl.Quotient[T] {
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
}
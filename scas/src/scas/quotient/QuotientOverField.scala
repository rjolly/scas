package scas.quotient

import scala.compiletime.deferred
import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.ufd.PolynomialOverField
import scas.util.Conversion

trait QuotientOverField[T, C, M] extends Quotient[T, C, M] {
  given ring: PolynomialOverField[T, C, M] = deferred
  given Field[C] = ring.ring
  override def apply(x: Element[T]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ (c.lastCoefficient / d.lastCoefficient)
    Element(n / gcd, d / gcd)
  }

  extension (ring: PolynomialOverField[T, C, M]) def quotient() = this
}

object QuotientOverField {
  class Conv[T, C, M](using PolynomialOverField[T, C, M]) extends QuotientOverField[T, C, M] with Field.Conv[Element[T]] {
    given instance: Conv[T, C, M] = this
  }
}

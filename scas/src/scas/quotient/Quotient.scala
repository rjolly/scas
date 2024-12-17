package scas.quotient

import scala.compiletime.deferred
import scas.structure.commutative.Quotient.Element
import scas.polynomial.ufd.{PolynomialOverUFD, PolynomialOverField}
import scas.util.Conversion

trait Quotient[T, C, M] extends scas.structure.commutative.Quotient[T] {
  given ring: PolynomialOverUFD[T, C, M] = deferred
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
  def toMathML = ring.toMathML(true)
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly

  extension (ring: PolynomialOverUFD[T, C, M]) def quotient() = this
}

object Quotient {
  def apply[T, C, M](ring: PolynomialOverField[T, C, M]) = new QuotientOverField.Conv(using ring)
}

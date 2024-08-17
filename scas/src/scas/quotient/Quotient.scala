package scas.quotient

import scas.structure.commutative.Quotient.Element
import scas.polynomial.{PolynomialOverUFD, PolynomialOverField}
import scas.util.Conversion

abstract class Quotient[T, C, M] extends scas.structure.commutative.Quotient[T] {
  given ring: PolynomialOverUFD[T, C, M]
  extension (x: Element[T]) def convert = {
    val Element(n, d) = x
    Element(n.convert, d.convert)
  }
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
  def toMathML = ring.toMathML(true)
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
}

object Quotient {
  def apply[T, C, M](ring: PolynomialOverField[T, C, M]) = new conversion.QuotientOverField(using ring)
}

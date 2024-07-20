package scas.quotient

import scas.polynomial.{PolynomialOverUFD, PolynomialOverField}
import scas.util.Conversion

trait Quotient[T, C, M] extends scas.structure.commutative.Quotient[T] {
  given ring: PolynomialOverUFD[T, C, M]
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
  def toMathML = ring.toMathML(true)
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
}

object Quotient {
  def apply[T, C, M](ring: PolynomialOverField[T, C, M]) = new conversion.Quotient(using ring)
}

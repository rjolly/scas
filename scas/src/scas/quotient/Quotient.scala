package scas.quotient

import scas.polynomial.{PolynomialOverUFD, PolynomialOverField}

trait Quotient[T, C, M](using ring: PolynomialOverUFD[T, C, M]) extends scas.structure.commutative.Quotient[T] {
  def generator(n: Int) = this(ring.generator(n))
  def generators = ring.generators.map(apply)
  def toMathML = ring.toMathML(true)
  export ring.coef2poly
}

object Quotient {
  def apply[T, C, M](ring: PolynomialOverField[T, C, M]) = new conversion.RationalFunction(using ring)
}

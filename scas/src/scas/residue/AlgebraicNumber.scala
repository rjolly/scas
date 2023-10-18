package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}

class AlgebraicNumber[T, C, M](using ring: scas.polynomial.ufd.UnivariatePolynomial[T, C, M]) extends impl.AlgebraicNumber[T, C, M] with Field[T] {
  given instance: AlgebraicNumber[T, C, M] = this
  given coef2poly[D: Conversion[C]]: (D => T) = x => ring(~x)
}

object AlgebraicNumber {
  def apply[C](ring: Field[C])(s: String*): AlgebraicNumber[Element[C, Array[Int]], C, Array[Int]] = apply(UnivariatePolynomial(ring)(s: _*))
  def apply[T, C, M](ring: scas.polynomial.ufd.UnivariatePolynomial[T, C, M]) = new AlgebraicNumber(using ring)
}

package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.{UnivariatePolynomial, MultivariatePolynomial}
import scas.polynomial.PolynomialOverUFD
import scas.base.{BigInteger, Rational}
import BigInteger.given
import Rational.given

trait RationalFunction[T, M](using ring: PolynomialOverUFD[T, BigInteger, M]) extends Quotient[T, BigInteger, M] {
  import ring.{degree, headCoefficient}
  extension (x: Element[T]) override def toCode(level: Level) = {
    val Element(n, d) = x
    if(degree(n) >< 0 && degree(d) >< 0) Rational(headCoefficient(n), headCoefficient(d)).toCode(level) else super.toCode(x)(level)
  }
  extension (x: Element[T]) override def toMathML = {
    val Element(n, d) = x
    if(degree(n) >< 0 && degree(d) >< 0) Rational(headCoefficient(n), headCoefficient(d)).toMathML else super.toMathML(x)
  }
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new conversion.RationalFunctionOverField(using UnivariatePolynomial(ring)(s*))
  def integral(s: String*) = new conversion.RationalFunction(using MultivariatePolynomial(BigInteger)(s*))
}

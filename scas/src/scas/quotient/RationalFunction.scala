package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.{MultivariatePolynomial, PolynomialWithSubresGCD}
import scas.polynomial.PolynomialOverUFD
import scas.base.{BigInteger, Rational}
import scas.variable.Variable
import scas.util.Conversion
import BigInteger.given
import Rational.given

class RationalFunction(using val ring: PolynomialOverUFD[MultivariatePolynomial.Element[BigInteger], BigInteger, Array[Int]]) extends Quotient[MultivariatePolynomial.Element[BigInteger], BigInteger, Array[Int]] {
  def this(s: Variable*) = this(using new PolynomialWithSubresGCD(using BigInteger)(s*))
  extension (x: Element[MultivariatePolynomial.Element[BigInteger]]) override def toCode(level: Level) = {
    val Element(n, d) = x
    if(n.degree >< 0 && d.degree >< 0) Rational(n.headCoefficient, d.headCoefficient).toCode(level) else super.toCode(x)(level)
  }
  extension (x: Element[MultivariatePolynomial.Element[BigInteger]]) override def toMathML = {
    val Element(n, d) = x
    if(n.degree >< 0 && d.degree >< 0) Rational(n.headCoefficient, d.headCoefficient).toMathML else super.toMathML(x)
  }
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.RationalFunctionOverField(ring)(s*)
  def integral[S : Conversion[Variable]](s: S*) = new conversion.RationalFunction(s*)
}

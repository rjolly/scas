package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.base.{BigInteger, Rational}
import scas.variable.Variable
import scas.util.Conversion
import BigInteger.given
import Rational.given

trait RationalFunction[T, M] extends Quotient[T, BigInteger, M] {
  def degree(x: T) = ring.degree(x)
  def headCoefficient(x: T) = ring.headCoefficient(x)
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
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.RationalFunctionOverField(using ring)(s*)
  def integral[S : Conversion[Variable]](s: S*) = new conversion.RationalFunction(s*)
}

package scas.quotient

import scas.structure.commutative.Quotient.Element
import scas.base.{BigInteger, Rational}
import BigInteger.given
import Rational.given

trait QuotientOverInteger[T, M] extends Quotient[T, BigInteger, M] {
  extension (x: Element[T]) override def toCode(level: Level) = {
    val Element(n, d) = x
    if(n.degree >< 0 && d.degree >< 0) Rational(n.headCoefficient, d.headCoefficient).toCode(level) else super.toCode(x)(level)
  }
  extension (x: Element[T]) override def toMathML = {
    val Element(n, d) = x
    if(n.degree >< 0 && d.degree >< 0) Rational(n.headCoefficient, d.headCoefficient).toMathML else super.toMathML(x)
  }
}

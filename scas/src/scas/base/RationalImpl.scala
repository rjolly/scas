package scas.base

import scas.structure.Quotient
import scas.structure.ordered.Field
import scas.{BigInteger, Rational, int2bigInt, bigInt2rational}

class RationalImpl extends Quotient[BigInteger] with Field[Rational]
  def compare(x: Rational, y: Rational) = {
    val (a, b) = x
    val (c, d) = y
    BigInteger.compare(a * d, c * b)
  }
  override def signum(x: Rational) = super[Quotient].signum(x)
  override def zero = 0
  override def one = 1

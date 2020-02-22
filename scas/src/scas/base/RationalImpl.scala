package scas.base

import scas.math.Equiv
import scas.structure.Quotient
import scas.{BigInteger, Rational, int2bigInt, bigInt2rational}

class RationalImpl extends Quotient[BigInteger] with Equiv[Rational]
  def apply(a: BigInteger, b: BigInteger) = (a, b)
  def equiv(x: Rational, y: Rational) = x match
    case (a, b) => y match
      case (c, d) => a >< c && b >< d
  override def (x: Rational) isZero = x >< 0
  override def (x: Rational) isOne = x >< 1
  override def zero = 0
  override def one = 1

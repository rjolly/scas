package scas.base

import scas.structure.Quotient
import scas.{BigInteger, Rational, int2bigInt, bigInt2rational}

class RationalImpl extends Quotient[BigInteger]
  def apply(a: BigInteger, b: BigInteger) = (a, b)
  def equiv(x: Rational, y: Rational) = x match
    case (a, b) => y match
      case (c, d) => a >< c && b >< d
  override def zero = 0
  override def one = 1

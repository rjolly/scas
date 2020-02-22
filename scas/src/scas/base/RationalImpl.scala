package scas.base

import scas.{BigInteger, Rational}
import scas.math.Equiv
import scas.structure.Quotient

class RationalImpl extends Quotient[BigInteger] with Equiv[Rational]
  def apply(a: BigInteger, b: BigInteger) = (a, b)
  def equiv(x: Rational, y: Rational) = x match
    case (a, b) => y match
      case (c, d) => a >< c && b >< d

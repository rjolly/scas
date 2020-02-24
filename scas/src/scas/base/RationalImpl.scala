package scas.base

import scas.structure.Quotient
import scas.{BigInteger, Rational, int2bigInt, bigInt2rational}

class RationalImpl extends Quotient[BigInteger]
  def apply(n: BigInteger, d: BigInteger) = (n, d)
  def equiv(x: Rational, y: Rational) = {
    val (a, b) = x
    val (c, d) = y
    a >< c && b >< d
  }
  override def zero = 0
  override def one = 1

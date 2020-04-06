package scas.base

import scas.structure.commutative.Quotient
import scas.structure.commutative.ordered.Field
import scas.{BigInteger, Rational, int2bigInt, bigInt2rational}

class RationalImpl extends Quotient[BigInteger] with Field[Rational] {
  def compare(x: Rational, y: Rational) = {
    val (a, b) = x
    val (c, d) = y
    BigInteger.compare(a * d, c * b)
  }
  override def signum(x: Rational) = super[Quotient].signum(x)
  def (x: Rational).toCode(level: Level) = {
    val (n, d) = x
    if (d >< 1) n.toCode(level) else {
      if (n.bitLength < 64 && d.bitLength < 64) n.toCode(level) + " %%" + d.toCode(level) else s"Rational($n, $d)"
    }
  }
  def (x: Rational).toMathML = {
    val (n, d) = x
    if (d >< 1) n.toMathML else s"""<cn type="rational">$n<sep/>$d</cn>"""
  }
  override lazy val zero = 0
  override lazy val one = 1
}

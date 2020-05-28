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
  override def (x: Rational).signum = super[Quotient].signum(x)
  override def (x: Rational).toCode(level: Level) = {
    val (n, d) = x
    if (d.isOne) n.toCode(level) else {
      if (n.bitLength < 64 && d.bitLength < 64) {
        val s = n.toCode(Level.Multiplication) + "%%" + d.toCode(Level.Power)
        if (level > Level.Multiplication) fenced(s) else s
      } else s"Rational($n, $d)"
    }
  }
  override def toString = "Rational"
  override def (x: Rational).toMathML = {
    val (n, d) = x
    if (d.isOne) n.toMathML else s"""<cn type="rational">$n<sep/>$d</cn>"""
  }
  override def toMathML = "<rationals/>"
  override lazy val zero = 0
  override lazy val one = 1
}

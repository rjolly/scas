package scas.base

import scas.structure.commutative.ordered.conversion.Quotient
import scas.structure.commutative.Quotient.Element
import BigInteger.given

type Rational = Element[BigInteger]

object Rational extends Quotient[BigInteger] {
  given Rational.type = this
  def apply(n: String, d: String): Rational = this(BigInteger(n), BigInteger(d))
  extension (a: Long) def %%(b: Long) = this(long2bigInt(a), long2bigInt(b))
  extension (x: Rational) override def toCode(level: Level) = {
    val Rational(n, d) = x
    if (d.isOne) n.toCode(level) else {
      if (n.bitLength < 64 && d.bitLength < 64) {
        val s = n.toCode(Level.Multiplication) + "%%" + d.toCode(Level.Power)
        if (level > Level.Multiplication) fenced(s) else s
      } else s"Rational($n, $d)"
    }
  }
  override def toString = "Rational"
  extension (x: Rational) override def toMathML = {
    val Rational(n, d) = x
    if (d.isOne) n.toMathML else s"""<cn type="rational">$n<sep/>$d</cn>"""
  }
  override def toMathML = "<rationals/>"
  override val zero = Rational(0)
  override val one = Rational(1)
}

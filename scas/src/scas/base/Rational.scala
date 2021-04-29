package scas.base

import scas.structure.commutative.ordered.Quotient
import scas.structure.commutative.Quotient.Element
import BigInteger.given
import Rational.Impl

type Rational = Element[BigInteger]

object Rational extends Impl {
  given Rational.type = this
  class Impl extends Quotient[BigInteger] {
    def apply(n: String): Rational = this(BigInteger(n))
    def apply(n: String, d: String): Rational = this(BigInteger(n), BigInteger(d))
    override val zero = Rational("0")
    override val one = Rational("1")
    extension (x: Rational) override def toCode(level: Level) = {
      val Rational(n, d) = x
      if (d.isOne) n.toCode(level) else {
        if (n.bitLength < 64 && d.bitLength < 64) {
          val s = n.toCode(Level.Multiplication) + "%%" + d.toCode(Level.Power)
          if (level > Level.Multiplication) fenced(s) else s
        } else s"Rational(\"$n\", \"$d\")"
      }
    }
    override def toString = "Rational"
    extension (x: Rational) override def toMathML = {
      val Rational(n, d) = x
      if (d.isOne) n.toMathML else s"""<cn type="rational">$n<sep/>$d</cn>"""
    }
    override def toMathML = "<rationals/>"
  }
}

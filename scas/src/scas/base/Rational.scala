package scas.base

import scas.structure.commutative.ordered.{Quotient, Field}
import scas.structure.commutative.Quotient.Element
import scas.util.{Conversion, unary_~}

type Rational = Element[BigInteger]

object Rational extends Rational.Impl with Field.Conv[Rational] {
  given instance: Rational.type = this
  class Impl extends Quotient[BigInteger] {
    override given ring: BigInteger.Impl = BigInteger
    def apply(n: String): Rational = this(BigInteger(n))
    def apply(n: String, d: String): Rational = this(BigInteger(n), BigInteger(d))
    override val zero = this("0")
    override val one = this("1")
    extension (x: Rational) override def toCode(level: Level) = {
      import Level.given
      val Rational(n, d) = x
      if d.isOne then n.toCode(level) else {
        if n.bitLength < 64 && d.bitLength < 64 then {
          val s = n.toCode(Level.Multiplication) + "%%" + d.toCode(Level.Power)
          if level > Level.Multiplication then fenced(s) else s
        } else s"Rational(\"$n\", \"$d\")"
      }
    }
    override def toString = "Rational"
    extension (x: Rational) override def toMathML = {
      val Rational(n, d) = x
      if d.isOne then n.toMathML else s"""<cn type="rational">$n<sep/>$d</cn>"""
    }
    def toMathML = "<rationals/>"

    extension (ring: BigInteger.Impl) def quotient() = this
  }
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(~a, ~b)
  }
}

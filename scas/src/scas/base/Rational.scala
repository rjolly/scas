package scas.base

import scas.structure.commutative.ordered.Quotient
import scas.structure.commutative.Quotient.Element
import scas.structure.commutative.ordered.conversion.Field
import scas.util.{Conversion, unary_~}

type Rational = Element[BigInteger]

object Rational extends Rational.Impl with Field[Rational] {
  given instance: Rational.type = this
  object Implicits {
    export Rational.{instance, ring2quotient}
  }
  class Impl extends Quotient[BigInteger] {
    given ring: BigInteger.type = BigInteger
    def apply(n: String): Rational = this(BigInteger(n))
    def apply(n: String, d: String): Rational = this(BigInteger(n), BigInteger(d))
    extension (x: Rational) override def toCode(level: Level) = {
      import Level.given
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
    def toMathML = "<rationals/>"
  }
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(~a, ~b)
  }
}

package scas.base

import scas.structure.Quotient
import scas.{int2bigInteger, long2bigInteger}
import scas.Implicits.{ZZ, infixUFDOps}
import Predef.{any2stringadd => _, _}
import Quotient.Element

object Rational extends Quotient[java.math.BigInteger] {
  override def apply(l: Long) = apply(l, 1)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val n = new java.math.BigInteger(numbits, rnd)
    val d = new java.math.BigInteger(numbits, rnd)
    reduce(if (rnd.nextBoolean()) -n else n, d + 1)
  }
  override def compare(x: Element[java.math.BigInteger], y: Element[java.math.BigInteger]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    ring.compare(a * d, c * b)
  }
  override def toCode(x: Element[java.math.BigInteger], precedence: Int) = {
    val Element(n, d) = x
    if (ring.isOne(d)) ring.toCode(n, precedence)
    else "frac(" + ring.toCode(n, 0) + ", " + ring.toCode(d, 0) + ")"
  }
  override def toString = "QQ"
}

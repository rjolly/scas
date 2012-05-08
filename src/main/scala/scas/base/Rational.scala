package scas.base

import scas.structure.Quotient
import scas.{int2bigInteger, long2bigInteger}
import scas.Implicits.{ZZ, infixUFDOps}
import Quotient.Element

object Rational extends Quotient[(java.math.BigInteger, java.math.BigInteger), java.math.BigInteger] {
  val ring = ZZ
  def apply(n: java.math.BigInteger, d: java.math.BigInteger) = if (ring.signum(d) < 0) (-n, -d) else (n, d)
  def apply(n: String, d: String): (java.math.BigInteger, java.math.BigInteger) = apply(BigInteger(n), BigInteger(d))
  def apply(s: String): (java.math.BigInteger, java.math.BigInteger) = apply(BigInteger(s))
  override def apply(l: Long) = apply(l, 1)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val n = new java.math.BigInteger(numbits, rnd)
    val d = new java.math.BigInteger(numbits, rnd)
    reduce(if (rnd.nextBoolean()) -n else n, d + BigInteger(1))
  }
  override def compare(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    ring.compare(a * d, c * b)
  }
  override def toCode(x: (java.math.BigInteger, java.math.BigInteger), precedence: Int) = {
    val (n, d) = x
    if (d.isOne) n.toCode(precedence)
    else {
      if (n.bitLength < 64 && d.bitLength < 64) "frac(" + n.toCode(0) + ", " + d.toCode(0) + ")"
      else "Rational(\"" + n + "\", \"" + d + "\")"
    }
  }
  override def toString = "QQ"
  override def toMathML(x: (java.math.BigInteger, java.math.BigInteger)) = {
    val (n, d) = x
    if (d.isOne) n.toMathML
    else <cn type="rational">{n}<sep/>{d}</cn>
  }
  override def toMathML = <rationals/>
}

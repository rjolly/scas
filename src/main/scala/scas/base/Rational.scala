package scas.base

import scas.structure.Quotient
import scas.{int2bigInteger, long2bigInteger}
import scas.Implicits.{ZZ, infixUFDOps}

object Rational extends Quotient[Rational, BigInteger] {
  val ring = ZZ
  def apply(n: BigInteger, d: BigInteger) = if (ring.signum(d) < 0) (-n, -d) else (n, d)
  override def apply(l: Long) = apply(l, 1)
  def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val n = new BigInteger(numbits, rnd)
    val d = new BigInteger(numbits, rnd)
    reduce(if (rnd.nextBoolean()) -n else n, d + BigInteger(1))
  }
  override def compare(x: Rational, y: Rational) = {
    val (a, b) = x
    val (c, d) = y
    ring.compare(a * d, c * b)
  }
  override def toCode(x: Rational, precedence: Int) = {
    val (n, d) = x
    if (d.isOne) n.toCode(precedence)
    else {
      if (n.bitLength < 64 && d.bitLength < 64) "frac(" + n.toCode(0) + ", " + d.toCode(0) + ")"
      else "Rational(\"" + n + "\", \"" + d + "\")"
    }
  }
  override def toString = "QQ"
  override def toMathML(x: Rational) = {
    val (n, d) = x
    if (d.isOne) n.toMathML
    else <cn type="rational">{n}<sep/>{d}</cn>
  }
  override def toMathML = <rationals/>
}

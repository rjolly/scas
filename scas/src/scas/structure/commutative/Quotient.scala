package scas.structure.commutative

import scas.{BigInteger, int2bigInt}

abstract class Quotient[T: UniqueFactorizationDomain] extends Field[(T, T)] {
  def ring = UniqueFactorizationDomain[T]
  def apply(n: T, d: T) = {
    val c = ring.gcd(n, d)
    val gcd = if (d.signum == -c.signum) -c else c
    (n / gcd, d / gcd)
  }
  def apply(n: T): (T, T) = (n, ring.one)
  def (x: (T, T)) + (y: (T, T)) = {
    val (a, b) = x
    val (c, d) = y
    val (b0, d0) = this(b, d)
    this(a * d0 + c * b0, b0 * d)
  }
  def (x: (T, T)) - (y: (T, T)) = {
    val (a, b) = x
    val (c, d) = y
    val (b0, d0) = this(b, d)
    this(a * d0 - c * b0, b0 * d)
  }
  def (x: (T, T)) * (y: (T, T)) = {
    val (a, b) = x
    val (c, d) = y
    val (a0, d0) = this(a, d)
    val (c0, b0) = this(c, b)
    (a0 * c0, b0 * d0)
  }
  def equiv(x: (T, T), y: (T, T)) = {
    val (a, b) = x
    val (c, d) = y
    a >< c && b >< d
  }
  def inverse(x: (T, T)) = {
    val (n, d) = x
    (d, n)
  }
  override def gcd(x: (T, T), y: (T, T)) = {
    val (a, b) = x
    val (c, d) = y
    (ring.gcd(a, c), ring.lcm(b, d))
  }
  override def (x: (T, T)).unary_- = {
    val (n, d) = x
    (-n, d)
  }
  override def (a: (T, T)) \ (b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else {
    val (n, d) = a
    (n \ b, d \ b)
  }
  override def abs(x: (T, T)) = {
    val (n, d) = x
    (ring.abs(n), d)
  }
  def (x: (T, T)).signum = {
    val (n, _) = x
    n.signum
  }
  def characteristic = ring.characteristic
  def (x: (T, T)).toCode(level: Level) = {
    val (n, d) = x
    if (d.isOne) n.toCode(level) else {
      val s = n.toCode(Level.Multiplication) + "/" + d.toCode(Level.Power)
      if (level > Level.Multiplication) s"($s)" else s
    }
  }
  override def toString = s"$ring/$ring"
  def (x: (T, T)).toMathML = {
    val (n, d) = x
    if (d.isOne) n.toMathML else s"<apply><divide/>${n.toMathML}${d.toMathML}</apply>"
  }
  def toMathML = s"<apply><divide/>${ring.toMathML}${ring.toMathML}</apply>"
  def zero = this(ring.zero)
  def one = this(ring.one)
}

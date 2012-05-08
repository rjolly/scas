package scas.structure

import scas.Implicits.infixUFDOps

trait Quotient[T <: Product2[R, R], R] extends Field[T] {
  implicit val ring: UniqueFactorizationDomain[R]
  def convert(x: T) = {
    val (n, d) = (x._1, x._2)
    reduce(ring.convert(n), ring.convert(d))
  }
  def reduce(n: R, d: R) = {
    val gcd = ring.gcd(n, d)
    apply(n / gcd, d / gcd)
  }
  def apply(n: R, d: R): T
  def apply(n: R): T = apply(n, ring.one)
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val n = ring.random(numbits)
    val d = ring.random(numbits)
    reduce(if (rnd.nextBoolean()) -n else n, d + ring.one)
  }
  override def pow(x: T, exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else {
    val (n, d) = (x._1, x._2)
    apply(ring.pow(n, exp), ring.pow(d, exp))
  }
  override def abs(x: T) = {
    val (n, d) = (x._1, x._2)
    apply(ring.abs(n), d)
  }
  override def signum(x: T) = {
    val (n, d) = (x._1, x._2)
    ring.signum(n)
  }
  def characteristic = ring.characteristic
  def plus(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    val gcd = ring.gcd(b, d)
    val (b0, d0) = (b / gcd, d / gcd)
    reduce(a * d0 + c * b0, b0 * d)
  }
  def minus(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    val gcd = ring.gcd(b, d)
    val (b0, d0) = (b / gcd, d / gcd)
    reduce(a * d0 - c * b0, b0 * d)
  }
  def times(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    val gcd1 = ring.gcd(a, d)
    val gcd2 = ring.gcd(c, b)
    val (a0, d0) = (a / gcd1, d / gcd1)
    val (c0, b0) = (c / gcd2, b / gcd2)
    apply(a0 * c0, b0 * d0)
  }
  def inverse(x: T) = {
    val (n, d) = (x._1, x._2)
    apply(d, n)
  }
  override def gcd(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    apply(ring.gcd(a, c), ring.lcm(b, d))
  }
  override def lcm(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    apply(ring.lcm(a, c), ring.gcd(b, d))
  }
  override def negate(x: T) = {
    val (n, d) = (x._1, x._2)
    apply(-n, d)
  }
  def compare(x: T, y: T) = {
    val (a, b) = (x._1, x._2)
    val (c, d) = (y._1, y._2)
    val s = ring.compare(a, c)
    if (s < 0) -1
    else if (s > 0) 1
    else ring.compare(b, d)
  }
  override def toCode(x: T, precedence: Int) = {
    val (n, d) = (x._1, x._2)
    if (d.isOne) n.toCode(precedence)
    else {
      val s = n.toCode(2) + "/" + d.toCode(2)
      val fenced = precedence > 1
      if (fenced) "(" + s + ")" else s
    }
  }
  override def toString = ring.toString + "/" + ring.toString
  def toMathML(x: T) = {
    val (n, d) = (x._1, x._2)
    if (d.isOne) n.toMathML
    else <apply><divide/>{n.toMathML}{d.toMathML}</apply>
  }
  def toMathML = <apply><divide/>{ring.toMathML}{ring.toMathML}</apply>
}

object Quotient {
  trait Element[T <: Element[T, R], R] extends Product2[R, R] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: Quotient[T, R]
  }
}
